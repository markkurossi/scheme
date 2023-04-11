//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"sort"
)

// Env implements environment bindings.
type Env struct {
	Frames []*EnvFrame
}

type FrameType int

const (
	TypeStack FrameType = iota
	TypeEnv
)

func (ft FrameType) String() string {
	if ft == TypeStack {
		return "s"
	}
	return "e"
}

type FrameUsage int

const (
	FUFrame FrameUsage = iota
	FUArgs
	FULet
	FUValue
)

var frameUsages = map[FrameUsage]string{
	FUFrame: "F",
	FUArgs:  "A",
	FULet:   "L",
	FUValue: "V",
}

func (fu FrameUsage) String() string {
	name, ok := frameUsages[fu]
	if ok {
		return name
	}
	return fmt.Sprintf("{FrameUsage %d}", fu)
}

// EnvFrame implements an environment frame.
type EnvFrame struct {
	Type     FrameType
	Usage    FrameUsage
	Index    int
	Size     int
	Bindings map[string]*EnvBinding
}

// EnvBinding defines symbol's location in the environment.
type EnvBinding struct {
	Frame    *EnvFrame
	Disabled bool
	Index    int
}

// NewEnv creates a new empty environment.
func NewEnv() *Env {
	return &Env{}
}

// Copy creates a new copy of the environment that shares the contents
// of the environment frames.
func (e *Env) Copy() *Env {
	frames := make([]*EnvFrame, len(e.Frames))
	copy(frames, e.Frames)

	return &Env{
		Frames: frames,
	}
}

// Print prints the environment to standard output.
func (e *Env) Print() {
	fmt.Printf("Env:\u2500\u2500\u252c\u2574depth=%v\n", len(e.Frames))
	for i := len(e.Frames) - 1; i >= 0; i-- {
		frame := e.Frames[i]

		fmt.Printf("\u2502%v%v:%3d", frame.Type, frame.Usage, i)
		for k, v := range frame.Bindings {
			fmt.Printf(" %v=%d.%d(%v)", k, frame.Index, v.Index, v.Disabled)
		}
		fmt.Println()
	}
	fmt.Printf("\u2570\u2500\u2500\u2500\u2500\u2500\u256f\n")
}

// Depth returns the depth of the environment.
func (e *Env) Depth() int {
	return len(e.Frames)
}

func (e *Env) PushCaptureFrame(captures bool, usage FrameUsage,
	size int) *EnvFrame {

	if captures {
		return e.PushFrame(TypeEnv, usage, size)
	}
	return e.PushFrame(TypeStack, usage, size)
}

// PushFrame pushes a new environment or stack frame based on frame
// type argument.
func (e *Env) PushFrame(t FrameType, usage FrameUsage, size int) *EnvFrame {
	var index int

	for _, f := range e.Frames {
		if f.Type == t {
			if t == TypeStack {
				index += f.Size
			} else {
				index++
			}
		}
	}

	frame := &EnvFrame{
		Type:     t,
		Usage:    usage,
		Index:    index,
		Size:     size,
		Bindings: make(map[string]*EnvBinding),
	}
	e.Frames = append(e.Frames, frame)
	return frame
}

// PopFrame pops the topmost environment frame.
func (e *Env) PopFrame() {
	e.Frames = e.Frames[:len(e.Frames)-1]
}

// Define defines the named symbol in the environment.
func (e *Env) Define(name string) (*EnvBinding, error) {
	frame := e.Frames[len(e.Frames)-1]

	_, ok := frame.Bindings[name]
	if ok {
		return nil, fmt.Errorf("symbol %s already defined", name)
	}
	b := &EnvBinding{
		Frame: frame,
		Index: len(frame.Bindings),
	}
	frame.Bindings[name] = b
	return b, nil
}

// Lookup finds the symbol from the environment.
func (e *Env) Lookup(name string) (*EnvBinding, bool) {
	for i := len(e.Frames) - 1; i >= 0; i-- {
		b, ok := e.Frames[i].Bindings[name]
		if ok && !b.Disabled {
			return b, true
		}
	}
	return nil, false
}

// Push pushes the frames of the argument environment to the top of
// this environment.
func (e *Env) Push(o *Env) {
	for _, frame := range o.Frames {
		var names []string
		for k := range frame.Bindings {
			names = append(names, k)
		}
		sort.Slice(names, func(i, j int) bool {
			return frame.Bindings[names[i]].Index <
				frame.Bindings[names[j]].Index
		})

		e.PushFrame(frame.Type, frame.Usage, len(names))
		for _, name := range names {
			e.Define(name)
		}
	}
}

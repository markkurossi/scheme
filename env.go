//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

// Env implements environment bindings.
type Env struct {
	Stats  *EnvStats
	Frames []*EnvFrame
}

// EnvStats define environment statistics.
type EnvStats struct {
	MaxStack int
}

// FrameType defines the frame type.
type FrameType int

// Frame types.
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

// FrameUsage defines how frame is used.
type FrameUsage int

// Frame usages.
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
	Type     *types.Type
}

// NewEnv creates a new empty environment.
func NewEnv() *Env {
	return &Env{
		Stats: new(EnvStats),
	}
}

// Copy creates a new copy of the environment that shares the contents
// of all environment frames and statistics.
func (e *Env) Copy() *Env {
	frames := make([]*EnvFrame, len(e.Frames))
	copy(frames, e.Frames)

	return &Env{
		Stats:  e.Stats,
		Frames: frames,
	}
}

// CopyEnvFrames creates a new copy of the environment sharing TypeEnv
// frames and statistics.
func (e *Env) CopyEnvFrames() *Env {
	var frames []*EnvFrame
	for _, frame := range e.Frames {
		if frame.Type == TypeEnv {
			frames = append(frames, frame)
		}
	}
	return &Env{
		Stats:  e.Stats,
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
			fmt.Printf(" %v=%d.%d(%v) %v",
				k, frame.Index, v.Index, v.Disabled, v.Type)
		}
		fmt.Println()
	}
	fmt.Printf("\u2570\u2500\u2500\u2500\u2500\u2500\u256f\n")
}

// Depth returns the depth of the environment.
func (e *Env) Depth() int {
	return len(e.Frames)
}

// PushCaptureFrame pushes a new stack frame. The capture argument
// specifies if the frame is an env or a stack frame.
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

	if t == TypeStack && index+size > e.Stats.MaxStack {
		e.Stats.MaxStack = index + size
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
func (e *Env) Define(name string, t *types.Type) (*EnvBinding, error) {
	frame := e.Frames[len(e.Frames)-1]

	_, ok := frame.Bindings[name]
	if ok {
		return nil, fmt.Errorf("symbol %s already defined", name)
	}
	b := &EnvBinding{
		Frame: frame,
		Index: len(frame.Bindings),
		Type:  t,
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

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
	"os"
	"sort"
)

// CompileFile compiles the Scheme file.
func (vm *VM) CompileFile(file string) (Code, error) {
	in, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer in.Close()
	parser := NewParser(file, in)

	vm.compiled = nil
	vm.env = NewEnv()

	for {
		v, err := parser.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			}
			break
		}

		err = vm.compileValue(v)
		if err != nil {
			return nil, err
		}
	}
	vm.addInstr(OpHalt, nil, 0)

	// Compile lambdas.
	for i := 0; i < len(vm.lambdas); i++ {
		lambda := vm.lambdas[i]

		vm.env = lambda.Env

		// Lambda body starts after the label.
		ofs := len(vm.compiled)
		lambda.Start = ofs + 1
		vm.addInstr(OpLabel, nil, lambda.Start)
		err = Map(vm.compileValue, lambda.Body)
		if err != nil {
			return nil, err
		}
		vm.addInstr(OpReturn, nil, 0)
		lambda.End = len(vm.compiled)

		vm.env = nil
	}

	// Patch lambda code offsets.
	for i := 0; i < len(vm.compiled); i++ {
		instr := vm.compiled[i]

		if instr.Op == OpLambda {
			def := vm.lambdas[instr.I]
			instr.I = def.Start
			instr.J = def.End
			instr.V = &Lambda{
				MinArgs: len(def.Args),
				MaxArgs: len(def.Args),
				Args:    def.Args,
				Code:    vm.compiled[def.Start:def.End],
				Body:    def.Body,
				Capture: def.Env.Depth() - 1,
			}
		}
	}

	return vm.compiled, nil
}

func (vm *VM) compileValue(value Value) error {
	switch v := value.(type) {
	case Pair:
		length, ok := ListLength(v)
		if !ok {
			return fmt.Errorf("compile value: %v", v)
		}
		if isKeyword(v.Car(), KwDefine) {
			return vm.compileDefine(v, length)
		}
		if isKeyword(v.Car(), KwLambda) {
			return vm.compileLambda(false, v, length)
		}
		if isKeyword(v.Car(), KwSet) {
			return vm.compileSet(v, length)
		}

		// Function call.

		// Compile function.
		err := vm.compileValue(v.Car())
		if err != nil {
			return err
		}

		// Create a call frame.
		vm.addInstr(OpPushF, vm.accu, 0)
		vm.env.PushFrame()

		// Push argument scope.
		vm.addInstr(OpPushS, nil, length-1)
		vm.env.PushFrame()

		// Evaluate arguments.
		li := v.Cdr()
		for j := 0; li != nil; j++ {
			pair, ok := li.(Pair)
			if !ok {
				return fmt.Errorf("invalid list: %v", li)
			}
			err := vm.compileValue(pair.Car())
			if err != nil {
				return err
			}
			li = pair.Cdr()
			instr := vm.addInstr(OpLocalSet, nil, vm.env.Depth()-1)
			instr.J = j
		}

		// Pop argument scope.
		vm.env.PopFrame()

		// Pop call frame.
		vm.env.PopFrame()

		// Function call. XXX tail-call
		vm.addInstr(OpCall, nil, 0)

		return nil

	case *Identifier:
		b, ok := vm.env.Lookup(v.Name)
		if ok {
			instr := vm.addInstr(OpLocal, nil, b.Frame)
			instr.J = b.Index
		} else {
			instr := vm.addInstr(OpGlobal, nil, 0)
			instr.Sym = vm.Intern(v.Name)
		}

	case Keyword:
		return fmt.Errorf("unexpected keyword: %s", v)

	case Boolean, String, Character, Number:
		vm.addInstr(OpConst, v, 0)

	default:
		return fmt.Errorf("compile value: %v(%T)", v, v)
	}
	return nil
}

func (vm *VM) addInstr(op Operand, v Value, i int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
	}
	vm.compiled = append(vm.compiled, instr)
	return instr
}

func (vm *VM) compileDefine(pair Pair, length int) error {
	// (define name value)
	name, _, ok := isIdentifier(Car(Cdr(pair, true)))
	if ok {
		if length != 3 {
			return fmt.Errorf("syntax error: %v", pair)
		}
		v, ok := Car(Cdr(Cdr(pair, true)))
		if !ok {
			return fmt.Errorf("syntax error: %v", pair)
		}
		err := vm.compileValue(v)
		if err != nil {
			return err
		}
		return vm.define(name.Name)
	}

	// (define (name args?) body)
	return vm.compileLambda(true, pair, length)
}

func (vm *VM) define(name string) error {
	nameSym := vm.Intern(name)
	_, ok := vm.env.Lookup(name)
	if ok || nameSym.Global != nil {
		return fmt.Errorf("symbol '%v' already defined", name)
	}

	if vm.env.Empty() {
		instr := vm.addInstr(OpDefine, nil, 0)
		instr.Sym = nameSym
	} else {
		b, err := vm.env.Define(name)
		if err != nil {
			return err
		}
		instr := vm.addInstr(OpLocalSet, nil, b.Frame)
		instr.J = b.Index
	}
	return nil
}

func (vm *VM) compileLambda(define bool, pair Pair, length int) error {
	// (define (name args?) body)
	// (lambda (args?) body)
	lst, ok := Car(Cdr(pair, true))
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}
	_, ok = ListLength(lst)
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}

	var name *Identifier
	var args []*Identifier

	err := Map(func(v Value) error {
		id, ok := v.(*Identifier)
		if !ok {
			return fmt.Errorf("lambda: arguments must be identifiers")
		}
		if define && name == nil {
			name = id
		} else {
			args = append(args, id)
		}
		return nil
	}, lst)
	if err != nil {
		return err
	}
	if define && name == nil {
		return fmt.Errorf("function name not define")
	}

	lst, ok = Cdr(Cdr(pair, true))
	if !ok {
		return fmt.Errorf("invalid function body")
	}

	var body Pair
	body, ok = lst.(Pair)
	if !ok {
		return fmt.Errorf("invalid function body")
	}

	// The environment (below) is converted to lambda capture:
	//
	//     0: let-frame-m           0: let-frame-m
	//        ...                      ...
	//   n-1: let-frame-0         n-2: let-frame-0
	//     n: ctx-function-args   n-1: ctx-function-args
	//							    n: lambda-args
	//
	// And the final lambda environment (scope) is as follows (the
	// lambda args are pushed to the top of the environment):
	//
	//     0: lambda-args
	//     1: let-frame-m
	//        ...
	//   n-1: let-frame-0
	//     n: ctx-function-args

	env := NewEnv()

	env.PushFrame()
	for _, arg := range args {
		_, err = env.Define(arg.Name)
		if err != nil {
			return err
		}
	}
	env.Push(vm.env)
	env.ShiftDown()

	vm.addInstr(OpLambda, nil, len(vm.lambdas))
	vm.lambdas = append(vm.lambdas, &LambdaBody{
		Args: args,
		Body: body,
		Env:  env,
	})
	if define {
		return vm.define(name.Name)
	}

	return nil
}

func (vm *VM) compileSet(pair Pair, length int) error {
	// (set! name value)
	if length != 3 {
		return fmt.Errorf("syntax error: %v", pair)
	}
	name, nameV, ok := isIdentifier(Car(Cdr(pair, true)))
	if !ok {
		return fmt.Errorf("set!: expected variable name: %v", nameV)
	}
	v, ok := Car(Cdr(Cdr(pair, true)))
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}
	err := vm.compileValue(v)
	if err != nil {
		return err
	}

	nameSym := vm.Intern(name.Name)
	b, ok := vm.env.Lookup(name.Name)
	if ok {
		instr := vm.addInstr(OpLocalSet, nil, b.Frame)
		instr.J = b.Index
	} else {
		instr := vm.addInstr(OpGlobalSet, nil, 0)
		instr.Sym = nameSym
	}

	return nil
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}

func isIdentifier(value Value, ok bool) (*Identifier, Value, bool) {
	if !ok {
		return nil, value, ok
	}
	id, ok := value.(*Identifier)
	return id, value, ok
}

// LambdaBody defines the lambda body and its location in the compiled
// bytecode.
type LambdaBody struct {
	Start int
	End   int
	Args  []*Identifier
	Body  Pair
	Env   *Env
}

// Env implements environment bindings.
type Env struct {
	Frames []EnvFrame
}

// NewEnv creates a new empty environment.
func NewEnv() *Env {
	return &Env{}
}

// Print prints the environment to standard output.
func (e *Env) Print() {
	fmt.Printf("Env:    depth=%v\n", len(e.Frames))
	for i := len(e.Frames) - 1; i >= 0; i-- {
		fmt.Printf("%7d", i)
		for k, v := range e.Frames[i] {
			fmt.Printf(" %v=%d.%d", k, v.Frame, v.Index)
		}
		fmt.Println()
	}
	fmt.Printf("-------\n")
}

// Empty tests if the environment is empty.
func (e *Env) Empty() bool {
	return e.Depth() == 0
}

// Depth returns the depth of the environment.
func (e *Env) Depth() int {
	return len(e.Frames)
}

// PushFrame pushes an environment frame.
func (e *Env) PushFrame() {
	e.Frames = append(e.Frames, make(EnvFrame))
}

// PopFrame pops the topmost environment frame.
func (e *Env) PopFrame() {
	e.Frames = e.Frames[:len(e.Frames)-1]
}

// ShiftDown shifts the environment frames down. The bottom-most
// elements goes to the top of the env.
func (e *Env) ShiftDown() {
	if len(e.Frames) < 2 {
		return
	}
	bottom := e.Frames[0]
	e.Frames = e.Frames[1:]
	e.Frames = append(e.Frames, bottom)
}

// Define defines the named symbol in the environment.
func (e *Env) Define(name string) (EnvBinding, error) {
	top := len(e.Frames) - 1
	_, ok := e.Frames[top][name]
	if ok {
		return EnvBinding{}, fmt.Errorf("symbol %s already defined", name)
	}
	b := EnvBinding{
		Frame: top,
		Index: len(e.Frames[top]),
	}
	e.Frames[top][name] = b
	return b, nil
}

// Lookup finds the symbol from the environment.
func (e *Env) Lookup(name string) (EnvBinding, bool) {
	for i := len(e.Frames) - 1; i >= 0; i-- {
		b, ok := e.Frames[i][name]
		if ok {
			return b, true
		}
	}
	return EnvBinding{}, false
}

// Push pushes the frames of the argument environment to the top of
// this environment.
func (e *Env) Push(o *Env) {
	for _, frame := range o.Frames {
		var names []string
		for k := range frame {
			names = append(names, k)
		}
		sort.Slice(names, func(i, j int) bool {
			return frame[names[i]].Index < frame[names[j]].Index
		})

		e.PushFrame()
		for _, name := range names {
			e.Define(name)
		}
	}
}

// EnvFrame implements an environment frame.
type EnvFrame map[string]EnvBinding

// EnvBinding defines symbol's location in the environment.
type EnvBinding struct {
	Frame int
	Index int
}

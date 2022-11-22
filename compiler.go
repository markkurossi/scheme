//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
)

// CompileFile compiles the Scheme file.
func (vm *VM) CompileFile(file string) (Code, error) {
	parser, err := NewParser(file)
	if err != nil {
		return nil, err
	}
	defer parser.Close()

	vm.compiled = nil
	vm.env = &Env{}

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
	for _, lambda := range vm.lambdas {
		// Define arguments.
		vm.env.PushFrame()
		for _, arg := range lambda.Args {
			_, err = vm.env.Define(arg.Name)
			if err != nil {
				return nil, err
			}
		}

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

		vm.env.PopFrame()
	}

	// Patch lambda code offsets.
	for i := 0; i < len(vm.compiled); i++ {
		instr := vm.compiled[i]

		if instr.Op == OpLambda {
			start := vm.lambdas[instr.I].Start
			end := vm.lambdas[instr.I].End
			instr.I = start
			instr.J = end
		}
	}

	return vm.compiled, nil
}

func (vm *VM) compileValue(value Value) error {
	switch v := value.(type) {
	case *Cons:
		length, ok := ListLength(v)
		if !ok {
			return fmt.Errorf("compile value: %v", v)
		}

		ok, err := vm.compileDefineFunc(v)
		if err != nil {
			return err
		}
		if ok {
			break
		}

		// Function call.

		// Compile function.
		err = vm.compileValue(v.Car)
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
		li := v.Cdr
		for j := 0; li != nil; j++ {
			cons, ok := li.(*Cons)
			if !ok {
				return fmt.Errorf("invalid list: %v", li)
			}
			err := vm.compileValue(cons.Car)
			if err != nil {
				return err
			}
			li = cons.Cdr
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

func (vm *VM) compileDefineFunc(cons *Cons) (ok bool, err error) {

	if !isKeyword(cons.Car, KwDefine) {
		return
	}

	lst, ok := Car(Cdr(cons, true))
	if !ok {
		return
	}

	var name *Identifier
	var args []*Identifier

	err = Map(func(v Value) error {
		id, ok := v.(*Identifier)
		if !ok {
			return fmt.Errorf("lambda: arguments must be identifiers")
		}
		if name == nil {
			name = id
		} else {
			args = append(args, id)
		}
		return nil
	}, lst)
	if err != nil {
		return
	}
	if name == nil {
		ok = false
		return
	}

	lst, ok = Cdr(Cdr(cons, true))
	if !ok {
		return
	}

	var body *Cons
	body, ok = lst.(*Cons)
	if !ok {
		return
	}

	nameSym := vm.Intern(name.Name)
	_, ok = vm.env.Lookup(name.Name)
	if ok || nameSym.Global != nil {
		return false, fmt.Errorf("symbol '%s' already defined", name.Name)
	}

	vm.addInstr(OpLambda, nil, len(vm.lambdas))
	vm.lambdas = append(vm.lambdas, &LambdaBody{
		Args: args,
		Body: body,
	})

	if vm.env.Empty() {
		instr := vm.addInstr(OpDefine, nil, 0)
		instr.Sym = nameSym
	} else {
		b, err := vm.env.Define(name.Name)
		if err != nil {
			return false, err
		}
		instr := vm.addInstr(OpLocalSet, nil, b.Frame)
		instr.J = b.Index
	}

	return true, nil
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}

// Env implements environment bindings.
type Env struct {
	Frames []EnvFrame
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

// EnvFrame implements an environment frame.
type EnvFrame map[string]EnvBinding

// EnvBinding defines symbol's location in the environment.
type EnvBinding struct {
	Frame int
	Index int
}

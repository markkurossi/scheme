//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
	"math"
)

// Operand defines a Scheme bytecode instruction.
type Operand int

// Bytecode instructions.
const (
	OpConst Operand = iota
	OpLocal
	OpGlobal
	OpLocalSet
	OpGlobalSet
	OpArgSet
	OpNFrame
	OpCall
	OpHalt
)

var operands = map[Operand]string{
	OpConst:     "const",
	OpLocal:     "local",
	OpGlobal:    "global",
	OpLocalSet:  "local!",
	OpGlobalSet: "global!",
	OpArgSet:    "arg!",
	OpNFrame:    "nframe",
	OpCall:      "call",
	OpHalt:      "halt",
}

func (op Operand) String() string {
	name, ok := operands[op]
	if ok {
		return name
	}
	return fmt.Sprintf("{op %d}", op)
}

// Instr implementes a Scheme bytecode instruction.
type Instr struct {
	Op  Operand
	V   Value
	I   int
	Sym *Identifier
}

func (i Instr) String() string {
	switch i.Op {
	case OpConst, OpLocal:
		return fmt.Sprintf("%s\t%v", i.Op, i.V)

	case OpGlobal:
		return fmt.Sprintf("%s\t%v", i.Op, i.Sym)

	case OpLocalSet, OpGlobalSet, OpArgSet, OpNFrame:
		return fmt.Sprintf("%s\t%v", i.Op, i.I)

	default:
		return fmt.Sprintf(i.Op.String())
	}
}

// Code implements scheme bytecode.
type Code []*Instr

// VM implements a Scheme virtual machine.
type VM struct {
	compiled Code
	pc       int
	accu     Value
	stack    *Frame
	symbols  map[string]*Identifier
}

// NewVM creates a new Scheme virtual machine.
func NewVM() (*VM, error) {
	vm := &VM{
		symbols: make(map[string]*Identifier),
	}
	display := vm.Intern("display")
	display.Global = &Lambda{
		MinArgs: 1,
		MaxArgs: math.MaxInt,
		Native: func(vm *VM, args []Value) (Value, error) {
			for idx, arg := range args {
				if idx > 0 {
					fmt.Print(" ")
				}
				fmt.Printf("%v", arg)
			}
			return nil, nil
		},
	}

	newline := vm.Intern("newline")
	newline.Global = &Lambda{
		Native: func(vm *VM, args []Value) (Value, error) {
			fmt.Println()
			return nil, nil
		},
	}

	return vm, nil
}

// EvalFile evaluates the scheme file.
func (vm *VM) EvalFile(file string) (Value, error) {
	code, err := vm.CompileFile(file)
	if err != nil {
		return nil, err
	}
	return vm.Execute(code)
}

// CompileFile compiles the Scheme file.
func (vm *VM) CompileFile(file string) (Code, error) {
	parser, err := NewParser(file)
	if err != nil {
		return nil, err
	}
	defer parser.Close()

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

	return vm.compiled, nil
}

func (vm *VM) compileValue(value Value) error {
	switch v := value.(type) {
	case *Cons:
		length, ok := ListLength(v)
		if !ok {
			return fmt.Errorf("compile value: %v", v)
		}

		// Compile function.
		err := vm.compileValue(v.Car)
		if err != nil {
			return err
		}

		// Create a call frame.
		vm.addInstr(OpNFrame, nil, length-1)

		// Evaluate arguments.
		li := v.Cdr
		for i := 0; li != nil; i++ {
			cons, ok := li.(*Cons)
			if !ok {
				return fmt.Errorf("invalid list: %v", li)
			}
			err := vm.compileValue(cons.Car)
			if err != nil {
				return err
			}
			li = cons.Cdr
			vm.addInstr(OpArgSet, nil, i)
		}

		// Function call. XXX tail-call
		vm.addInstr(OpCall, nil, 0)

		return nil

	case *Identifier:
		// XXX local vs. global
		instr := vm.addInstr(OpGlobal, nil, 0)
		instr.Sym = vm.Intern(v.Name)

	case *Boolean, *String, *Character, Number:
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

// Execute runs the code.
func (vm *VM) Execute(code Code) (Value, error) {

	var err error

	for {
		instr := code[vm.pc]
		vm.pc++

		switch instr.Op {
		case OpConst:
			vm.accu = instr.V

		case OpGlobal:
			vm.accu = instr.Sym.Global

		case OpArgSet:
			vm.stack.Args[instr.I] = vm.accu

		case OpNFrame:
			f := &Frame{
				Next: vm.stack,
			}
			switch ac := vm.accu.(type) {
			case *Lambda:
				f.Lambda = ac
				if instr.I < f.Lambda.MinArgs {
					return nil, fmt.Errorf("too few arguments")
				}
				if instr.I > f.Lambda.MaxArgs {
					return nil, fmt.Errorf("too many arguments")
				}
				f.Args = make([]Value, instr.I, instr.I)

			default:
				return nil, fmt.Errorf("invalid function: %v", ac)
			}
			vm.stack = f

		case OpCall:
			lambda := vm.stack.Lambda
			if lambda.Native != nil {
				vm.accu, err = lambda.Native(vm, vm.stack.Args)
				if err != nil {
					return nil, err
				}
				vm.stack = vm.stack.Next
			} else {
				return nil, fmt.Errorf("call: %v", lambda)
			}

		case OpHalt:
			return vm.accu, nil

		default:
			return nil, fmt.Errorf("%s: not implemented", instr.Op)
		}
	}
}

// Intern interns the name and returns the interned symbol.
func (vm *VM) Intern(val string) *Identifier {
	id, ok := vm.symbols[val]
	if !ok {
		id = &Identifier{
			Name: val,
		}
		vm.symbols[val] = id
	}
	return id
}

// Frame implements a VM stack frame.
type Frame struct {
	Next   *Frame
	Lambda *Lambda
	Args   []Value
}

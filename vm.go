//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
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
	OpPushS
	OpPopS
	OpCall
	OpHalt
)

var operands = map[Operand]string{
	OpConst:     "const",
	OpLocal:     "local",
	OpGlobal:    "global",
	OpLocalSet:  "local!",
	OpGlobalSet: "global!",
	OpPushS:     "pushs",
	OpPopS:      "pops",
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
	J   int
	Sym *Identifier
}

func (i Instr) String() string {
	switch i.Op {
	case OpConst:
		return fmt.Sprintf("%s\t%v", i.Op, i.V)

	case OpPushS:
		return fmt.Sprintf("%s\t%v", i.Op, i.I)

	case OpLocal, OpLocalSet:
		return fmt.Sprintf("%s\t%v.%v", i.Op, i.I, i.J)

	case OpGlobal, OpGlobalSet:
		return fmt.Sprintf("%s\t%v", i.Op, i.Sym)

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

	vm.DefineBuiltins(outputBuiltins)
	vm.DefineBuiltins(stringBuiltins)

	return vm, nil
}

// DefineBuiltins defines the built-in functions, defined in the
// argument array.
func (vm *VM) DefineBuiltins(builtins []Builtin) {
	for _, bi := range builtins {
		vm.DefineBuiltin(bi.Name, bi.MinArgs, bi.MaxArgs, bi.Native)
	}
}

// DefineBuiltin defines a built-in native function.
func (vm *VM) DefineBuiltin(name string, minArgs, maxArgs int, native Native) {
	sym := vm.Intern(name)
	sym.Global = &Lambda{
		MinArgs: minArgs,
		MaxArgs: maxArgs,
		Native:  native,
	}
}

// EvalFile evaluates the scheme file.
func (vm *VM) EvalFile(file string) (Value, error) {
	code, err := vm.CompileFile(file)
	if err != nil {
		return nil, err
	}
	for _, c := range code {
		fmt.Printf("\t%s\n", c)
	}
	return vm.Execute(code)
}

// Execute runs the code.
func (vm *VM) Execute(code Code) (Value, error) {

	vm.pushFrame()
	var err error

	var stack [][]Value

	for {
		instr := code[vm.pc]
		vm.pc++

		switch instr.Op {
		case OpConst:
			vm.accu = instr.V

		case OpGlobal:
			vm.accu = instr.Sym.Global

		case OpLocalSet:
			stack[instr.I][instr.J] = vm.accu

		case OpPushS:
			stack = append(stack, make([]Value, instr.I, instr.I))

		case OpCall:
			frame := vm.pushFrame()
			stackTop := len(stack) - 1
			args := stack[stackTop]

			switch ac := vm.accu.(type) {
			case *Lambda:
				frame.Lambda = ac
				if len(args) < frame.Lambda.MinArgs {
					return nil, fmt.Errorf("too few arguments")
				}
				if len(args) > frame.Lambda.MaxArgs {
					return nil, fmt.Errorf("too many arguments")
				}

			default:
				return nil, fmt.Errorf("invalid function: %v", ac)
			}

			lambda := vm.stack.Lambda
			if lambda.Native != nil {
				vm.accu, err = lambda.Native(vm, args)
				if err != nil {
					return nil, err
				}
				vm.popFrame()
			} else {
				return nil, fmt.Errorf("call: %v", lambda)
			}
			stack = stack[:stackTop]

		case OpHalt:
			vm.popFrame()
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

func (vm *VM) pushFrame() *Frame {
	f := &Frame{
		Next: vm.stack,
	}
	vm.stack = f
	return f
}

func (vm *VM) popFrame() {
	vm.stack = vm.stack.Next
}

// Frame implements a VM call stack frame.
type Frame struct {
	Next   *Frame
	Lambda *Lambda
}

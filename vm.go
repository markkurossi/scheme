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
	OpDefine
	OpLambda
	OpLabel
	OpLocal
	OpGlobal
	OpLocalSet
	OpGlobalSet
	OpPushF
	OpPopF
	OpPushS
	OpPopS
	OpCall
	OpReturn
	OpHalt
)

var operands = map[Operand]string{
	OpConst:     "const",
	OpDefine:    "define",
	OpLambda:    "lambda",
	OpLabel:     "label",
	OpLocal:     "local",
	OpGlobal:    "global",
	OpLocalSet:  "local!",
	OpGlobalSet: "global!",
	OpPushF:     "pushf",
	OpPopF:      "popf",
	OpPushS:     "pushs",
	OpPopS:      "pops",
	OpCall:      "call",
	OpReturn:    "return",
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
	case OpLabel:
		return fmt.Sprintf(".l%v:", i.I)

	case OpConst:
		str := fmt.Sprintf("\t%s\t", i.Op)
		if i.V == nil {
			str += fmt.Sprintf("%v", i.V)
		} else {
			str += i.V.Scheme()
		}
		return str

	case OpPushF:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.I != 0)

	case OpPushS:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.I)

	case OpLambda:
		return fmt.Sprintf("\t%s\tl%v:%v", i.Op, i.I, i.J)

	case OpLocal, OpLocalSet:
		return fmt.Sprintf("\t%s\t%v.%v", i.Op, i.I, i.J)

	case OpGlobal, OpGlobalSet, OpDefine:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.Sym)

	default:
		return fmt.Sprintf("\t%s", i.Op.String())
	}
}

// Code implements scheme bytecode.
type Code []*Instr

// VM implements a Scheme virtual machine.
type VM struct {
	compiled Code
	env      *Env
	lambdas  []*LambdaBody

	pc      int
	fp      int
	accu    Value
	stack   [][]Value
	symbols map[string]*Identifier
}

// LambdaBody defines the lambda body and its location in the compiled
// bytecode.
type LambdaBody struct {
	Start int
	End   int
	Args  []*Identifier
	Body  Pair
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
		vm.DefineBuiltin(bi.Name, bi.Args, bi.Native)
	}
}

// DefineBuiltin defines a built-in native function.
func (vm *VM) DefineBuiltin(name string, args []string, native Native) {

	var minArgs, maxArgs int
	var usage []*Identifier

	for _, arg := range args {
		usage = append(usage, &Identifier{
			Name: arg,
		})
		maxArgs++
		if arg[0] != '[' {
			minArgs++
		}
	}

	sym := vm.Intern(name)
	sym.Global = &Lambda{
		Args:    usage,
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
		fmt.Printf("%s\n", c)
	}
	return vm.Execute(code)
}

// Execute runs the code.
func (vm *VM) Execute(code Code) (Value, error) {

	frame := vm.pushFrame(nil, true)
	vm.fp = frame.Index

	var err error

	for {
		instr := code[vm.pc]
		vm.pc++

		switch instr.Op {
		case OpConst:
			vm.accu = instr.V

		case OpDefine:
			instr.Sym.Global = vm.accu

		case OpLambda:
			vm.accu = instr.V

		case OpLocal:
			// fmt.Printf("*** local: %v.%v\n", vm.fp+1+instr.I, instr.J)
			// vm.printStack()
			vm.accu = vm.stack[vm.fp+1+instr.I][instr.J]

		case OpGlobal:
			vm.accu = instr.Sym.Global

		case OpLocalSet:
			// fmt.Printf("*** local! I=%v, J=%v\n", vm.fp+1+instr.I, instr.J)
			// vm.printStack()
			vm.stack[vm.fp+1+instr.I][instr.J] = vm.accu
			// fmt.Printf(" =>\n")
			// vm.printStack()

		case OpGlobalSet:
			if instr.Sym.Global == nil {
				return nil, fmt.Errorf("undefined symbol '%s'", instr.Sym.Name)
			}
			instr.Sym.Global = vm.accu

		case OpPushF:
			// i.I != 0 for toplevel frames.
			lambda, ok := vm.accu.(*Lambda)
			if !ok {
				return nil, fmt.Errorf("invalid function: %v", vm.accu)
			}
			vm.pushFrame(lambda, instr.I != 0)

		case OpPushS:
			vm.pushScope(instr.I)

		case OpCall:
			stackTop := len(vm.stack) - 1
			args := vm.stack[stackTop]

			callFrame, ok := vm.stack[stackTop-1][0].(*Frame)
			if !ok || callFrame.Lambda == nil {
				return nil, fmt.Errorf("invalid function: %v", vm.accu)
			}
			lambda := callFrame.Lambda

			if len(args) < lambda.MinArgs {
				return nil, fmt.Errorf("too few arguments")
			}
			if len(args) > lambda.MaxArgs {
				return nil, fmt.Errorf("too many arguments")
			}

			// Set fp form the call.
			vm.fp = stackTop - 1

			if lambda.Native != nil {
				vm.accu, err = callFrame.Lambda.Native(vm, args)
				if err != nil {
					return nil, err
				}
				vm.popFrame()
			} else {
				// Save current excursion.
				callFrame.PC = vm.pc
				callFrame.Code = code

				code = lambda.Code
				vm.pc = 0
			}

		case OpReturn:
			frame, ok := vm.stack[vm.fp][0].(*Frame)
			if !ok {
				return nil, fmt.Errorf("invalid function: %v", vm.accu)
			}
			vm.pc = frame.PC
			code = frame.Code
			vm.popFrame()

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

func (vm *VM) pushScope(size int) {
	vm.stack = append(vm.stack, make([]Value, size, size))
}

func (vm *VM) popScope() {
	vm.stack = vm.stack[:len(vm.stack)-1]
}

func (vm *VM) pushFrame(lambda *Lambda, toplevel bool) *Frame {
	// Check that frame is valid.
	if vm.fp < len(vm.stack) {
		if len(vm.stack[vm.fp]) != 1 {
			panic(fmt.Sprintf("invalid frame: %v", vm.stack[vm.fp]))
		}
		_, ok := vm.stack[vm.fp][0].(*Frame)
		if !ok {
			panic(fmt.Sprintf("invalid frame: %v", vm.stack[vm.fp][0]))
		}
	}

	f := &Frame{
		Index:    len(vm.stack),
		Lambda:   lambda,
		Toplevel: toplevel,
	}

	f.Next = vm.fp
	// vm.fp = len(vm.stack)

	vm.pushScope(1)
	vm.stack[f.Index][0] = f

	return f
}

func (vm *VM) popFrame() {
	// Check that frame is valid.
	if len(vm.stack[vm.fp]) != 1 {
		panic(fmt.Sprintf("invalid frame: %v", vm.stack[vm.fp]))
	}
	frame, ok := vm.stack[vm.fp][0].(*Frame)
	if !ok {
		panic(fmt.Sprintf("invalid frame: %v", vm.stack[vm.fp][0]))
	}
	vm.stack = vm.stack[:vm.fp]
	vm.fp = frame.Next
}

// Frame implements a VM call stack frame.
type Frame struct {
	Index    int
	Next     int
	Lambda   *Lambda
	PC       int
	Code     Code
	Toplevel bool
}

// Scheme returns the value as a Scheme string.
func (f *Frame) Scheme() string {
	return f.String()
}

func (f *Frame) String() string {
	return fmt.Sprintf("frame: next=%v, lambda=%v, toplevel=%v",
		f.Next, f.Lambda, f.Toplevel)
}

func (vm *VM) printStack() {
	for i := len(vm.stack) - 1; i >= 0; i-- {
		if vm.fp == i {
			fmt.Printf("%7d>%v\n", i, vm.stack[i])
		} else {
			fmt.Printf("%7d %v\n", i, vm.stack[i])
		}
	}
}

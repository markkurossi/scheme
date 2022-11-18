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

		name, args, body, ok, err := isDefineFunc(v)
		if err != nil {
			return err
		}
		if ok {
			fmt.Printf("(define (%s %v) %v\n", name, args, body)
			break
		}

		// Compile function.
		err = vm.compileValue(v.Car)
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

	case Keyword:
		return fmt.Errorf("unexpected keyword: %s", v)

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

	vm.pushFrame()
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
			f := vm.pushFrame()
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
				f.Locals = ac.Locals

			default:
				return nil, fmt.Errorf("invalid function: %v", ac)
			}

		case OpCall:
			lambda := vm.stack.Lambda
			if lambda.Native != nil {
				vm.accu, err = lambda.Native(vm, vm.stack.Args)
				if err != nil {
					return nil, err
				}
				vm.popFrame()
			} else {
				return nil, fmt.Errorf("call: %v", lambda)
			}

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

// Frame implements a VM stack frame.
type Frame struct {
	Next   *Frame
	Lambda *Lambda
	Args   []Value
	Locals []Value
}

func isDefineFunc(cons *Cons) (name *Identifier, args []*Identifier, body *Cons,
	ok bool, err error) {

	if !isKeyword(cons.Car, KwDefine) {
		return
	}

	lst, ok := Car(Cdr(cons, true))
	if !ok {
		return
	}
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
	body, ok = lst.(*Cons)

	return
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}

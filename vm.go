//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
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
	OpPushS
	OpPopS
	OpPushA
	OpCall
	OpIf
	OpIfNot
	OpJmp
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
	OpPushS:     "pushs",
	OpPopS:      "pops",
	OpPushA:     "pusha",
	OpCall:      "call",
	OpIf:        "if",
	OpIfNot:     "ifnot",
	OpJmp:       "jmp",
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

// Instr implements a Scheme bytecode instruction.
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

	case OpIf, OpIfNot, OpJmp:
		return fmt.Sprintf("\t%s\t%v\t; l%v", i.Op, i.I, i.J)

	case OpCall:
		var suffix string
		if i.I != 0 {
			suffix = "\ttail"
		}
		return fmt.Sprintf("\t%s%s", i.Op, suffix)

	default:
		return fmt.Sprintf("\t%s", i.Op.String())
	}
}

// Execute runs the module.
func (scm *Scheme) Execute(module *Module) (Value, error) {

	scm.pc = 0
	scm.accu = nil

	frame := scm.pushFrame(nil, true)
	frame.Module = module
	scm.fp = frame.Index

	var err error
	code := module.Init

	for {
		instr := code[scm.pc]
		scm.pc++

		switch instr.Op {
		case OpConst:
			scm.accu = instr.V

		case OpDefine:
			if instr.Sym.Flags&FlagDefined != 0 {
				return nil, scm.Breakf("symbol '%s' already defined",
					instr.Sym.Name)
			}
			instr.Sym.Global = scm.accu
			instr.Sym.Flags |= FlagDefined

		case OpLambda:
			tmpl, ok := instr.V.(*Lambda)
			if !ok {
				return nil, scm.Breakf("lambda: invalid argument: %V", instr.V)
			}
			lambda := &Lambda{
				Args:    tmpl.Args,
				Capture: tmpl.Capture,
				Native:  tmpl.Native,
				Source:  tmpl.Source,
				Code:    tmpl.Code,
				PCMap:   tmpl.PCMap,
				Body:    tmpl.Body,
			}
			for i := len(scm.stack) - tmpl.Capture; i < len(scm.stack); i++ {
				lambda.Locals = append(lambda.Locals, scm.stack[i])
			}
			scm.accu = lambda

		case OpLabel:

		case OpLocal:
			// fmt.Printf("*** local: %v.%v\n", scm.fp+1+instr.I, instr.J)
			// scm.printStack()
			scm.accu = scm.stack[scm.fp+1+instr.I][instr.J]

		case OpGlobal:
			if instr.Sym.Flags&FlagDefined == 0 {
				return nil, scm.Breakf("undefined symbol '%s' ", instr.Sym.Name)
			}
			scm.accu = instr.Sym.Global

		case OpLocalSet:
			// fmt.Printf("*** local! I=%v, J=%v\n", scm.fp+1+instr.I, instr.J)
			// scm.printStack()
			scm.stack[scm.fp+1+instr.I][instr.J] = scm.accu
			// fmt.Printf(" =>\n")
			// scm.printStack()

		case OpGlobalSet:
			if instr.Sym.Global == nil {
				return nil, scm.Breakf("undefined symbol '%s'", instr.Sym.Name)
			}
			instr.Sym.Global = scm.accu

		case OpPushF:
			// i.I != 0 for toplevel frames.
			lambda, ok := scm.accu.(*Lambda)
			if !ok {
				return nil, scm.Breakf("pushf: invalid function: %v(%T)",
					scm.accu, scm.accu)
			}
			scm.pushFrame(lambda, instr.I != 0)

		case OpPushS:
			scm.pushScope(instr.I)

		case OpPopS:
			scm.popScope()

		case OpPushA:
			var scope []Value
			err = Map(func(idx int, v Value) error {
				scope = append(scope, v)
				return nil
			}, scm.accu)
			if err != nil {
				return nil, scm.Breakf("pusha: invalid arguments: %v", err)
			}
			scm.stack = append(scm.stack, scope)

		case OpCall:
			stackTop := len(scm.stack) - 1
			args := scm.stack[stackTop]

			callFrame, ok := scm.stack[stackTop-1][0].(*Frame)
			if !ok || callFrame.Lambda == nil {
				return nil, scm.Breakf("call: invalid function: %v", scm.accu)
			}
			lambda := callFrame.Lambda

			if len(args) < lambda.Args.Min {
				return nil, scm.Breakf("too few arguments: got %v, need %v",
					len(args), lambda.Args.Min)
			}
			if len(args) > lambda.Args.Max {
				return nil, scm.Breakf("too many arguments: got %v, max %v",
					len(args), lambda.Args.Max)
			}

			// Set fp for the call.
			scm.fp = stackTop - 1

			// Save current excursion.
			callFrame.PC = scm.pc
			callFrame.Code = code

			if lambda.Native != nil {
				scm.accu, err = callFrame.Lambda.Native(scm, lambda, args)
				if err != nil {
					return nil, scm.Breakf("%v", err)
				}
				scm.pc = callFrame.PC
				code = callFrame.Code
				scm.popFrame()
			} else {
				// Handle rest arguments.
				if lambda.Args.Rest != nil {
					var rest Pair
					for i := len(args) - 1; i >= lambda.Args.Min; i-- {
						rest = NewPair(args[i], rest)
					}
					args = args[:lambda.Args.Min]
					args = append(args, rest)
					scm.stack[stackTop] = args
				}

				if instr.I != 0 {
					// Tail-call.

					next := callFrame.Next

					nextFrame, ok := scm.stack[next][0].(*Frame)
					if !ok {
						return nil, scm.Breakf("invalid next frame: %v",
							scm.stack[callFrame.Next])
					}

					nextFrame.Lambda = callFrame.Lambda
					scm.stack[next+1] = scm.stack[stackTop]
					scm.stack = scm.stack[:next+2]

					scm.fp = next
				}
				if len(lambda.Locals) > 0 {
					for _, frame := range lambda.Locals {
						scm.stack = append(scm.stack, frame)
					}
				}

				// Jump to lambda code.
				code = lambda.Code
				scm.pc = 0
			}

		case OpIf:
			if True(scm.accu) {
				scm.pc += instr.I
			}

		case OpIfNot:
			if !True(scm.accu) {
				scm.pc += instr.I
			}

		case OpJmp:
			scm.pc += instr.I

		case OpReturn:
			frame, ok := scm.stack[scm.fp][0].(*Frame)
			if !ok {
				return nil, scm.Breakf("return: invalid function: %v", scm.accu)
			}
			scm.pc = frame.PC
			code = frame.Code
			scm.popFrame()

		case OpHalt:
			scm.popFrame()
			return scm.accu, nil

		default:
			return nil, scm.Breakf("%s: not implemented", instr.Op)
		}
	}
}

// Breakf breaks the program execution with the error.
func (scm *Scheme) Breakf(format string, a ...interface{}) error {
	err := scm.VMErrorf(format, a...)
	if true {
		fmt.Printf("%s\n", err.Error())
		scm.StackTrace()
	}
	return err
}

// Location returns the source file location of the current VM
// continuation.
func (scm *Scheme) Location() (source string, line int, err error) {
	fp := scm.fp
	pc := scm.pc

	for {
		frame, ok := scm.stack[fp][0].(*Frame)
		if !ok {
			err = errors.New("no stack")
			return
		}
		source, line = frame.MapPC(pc)
		if line > 0 {
			return
		}
		if frame.Next == fp {
			break
		}
		fp = frame.Next
		pc = frame.PC
	}
	return
}

// VMErrorf creates a virtual machine error.
func (scm *Scheme) VMErrorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)

	source, line, err := scm.Location()
	if err != nil || line == 0 || len(source) == 0 {
		return errors.New(msg)
	} else if line == 0 {
		return fmt.Errorf("%s: %s", source, msg)
	}
	return fmt.Errorf("%s:%v: %s", source, line, msg)
}

// StackTrace prints the virtual machine stack.
func (scm *Scheme) StackTrace() {
	fp := scm.fp
	pc := scm.pc

	for {
		frame, ok := scm.stack[fp][0].(*Frame)
		if !ok {
			panic("corrupted stack")
		}

		fmt.Print("\t")
		if frame.Module != nil {
			fmt.Printf("\u25A1=%v", frame.Module.Source)
		} else if frame.Lambda != nil {
			fmt.Printf("\u03BB=%v", frame.Lambda.Signature(false))
		} else {
			fmt.Printf("???")
		}
		fmt.Printf(" at ")
		source, line := frame.MapPC(pc)
		if line > 0 {
			fmt.Printf("%s:%d, ", source, line)
		}
		fmt.Printf("pc=%v\n", pc)

		if frame.Next == fp {
			break
		}
		fp = frame.Next
		pc = frame.PC
	}
}

// Intern interns the name and returns the interned symbol.
func (scm *Scheme) Intern(val string) *Identifier {
	id, ok := scm.symbols[val]
	if !ok {
		id = &Identifier{
			Name: val,
		}
		scm.symbols[val] = id
	}
	return id
}

func (scm *Scheme) pushScope(size int) {
	scm.stack = append(scm.stack, make([]Value, size, size))
}

func (scm *Scheme) popScope() {
	scm.stack = scm.stack[:len(scm.stack)-1]
}

func (scm *Scheme) pushFrame(lambda *Lambda, toplevel bool) *Frame {
	// Check that frame is valid.
	if scm.fp < len(scm.stack) {
		if len(scm.stack[scm.fp]) != 1 {
			panic(fmt.Sprintf("invalid frame: %v", scm.stack[scm.fp]))
		}
		_, ok := scm.stack[scm.fp][0].(*Frame)
		if !ok {
			panic(fmt.Sprintf("invalid frame: %v", scm.stack[scm.fp][0]))
		}
	}

	f := &Frame{
		Index:    len(scm.stack),
		Lambda:   lambda,
		Toplevel: toplevel,
	}

	f.Next = scm.fp

	scm.pushScope(1)
	scm.stack[f.Index][0] = f

	return f
}

func (scm *Scheme) popFrame() {
	// Check that frame is valid.
	if len(scm.stack[scm.fp]) != 1 {
		panic(fmt.Sprintf("invalid frame: %v", scm.stack[scm.fp]))
	}
	frame, ok := scm.stack[scm.fp][0].(*Frame)
	if !ok {
		panic(fmt.Sprintf("invalid frame: %v", scm.stack[scm.fp][0]))
	}
	scm.stack = scm.stack[:scm.fp]
	scm.fp = frame.Next
}

// Frame implements a SCM call stack frame.
type Frame struct {
	Index    int
	Next     int
	Toplevel bool

	Lambda *Lambda
	Module *Module
	PC     int
	Code   Code
}

// Scheme returns the value as a Scheme string.
func (f *Frame) Scheme() string {
	return f.String()
}

// MapPC maps the program counter value to the source location.
func (f *Frame) MapPC(pc int) (source string, line int) {
	if f.Lambda != nil {
		return f.Lambda.MapPC(pc)
	}
	if f.Module != nil {
		return f.Module.MapPC(pc)
	}
	return
}

// Eq tests if the argument value is eq? to this value.
func (f *Frame) Eq(o Value) bool {
	return f == o
}

// Equal tests if the argument value is equal to this value.
func (f *Frame) Equal(o Value) bool {
	ov, ok := o.(*Frame)
	if !ok {
		return false
	}
	return f.Index == ov.Index &&
		f.Next == ov.Next &&
		f.Lambda == ov.Lambda &&
		f.PC == ov.PC &&
		f.Toplevel == ov.Toplevel
}

func (f *Frame) String() string {
	var toplevel = "#f"
	if f.Toplevel {
		toplevel = "#t"
	}
	return fmt.Sprintf("frame: next=%v, toplevel=%v, \u03BB=%v",
		f.Next, toplevel, f.Lambda)
}

func (scm *Scheme) printStack() {
	for i := len(scm.stack) - 1; i >= 0; i-- {
		if scm.fp == i {
			fmt.Printf("%7d>%v\n", i, scm.stack[i])
		} else {
			fmt.Printf("%7d %v\n", i, scm.stack[i])
		}
	}
}

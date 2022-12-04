//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

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
	OpPushA
	OpPopS
	OpCall
	OpIf
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
	OpPopF:      "popf",
	OpPushS:     "pushs",
	OpPushA:     "pusha",
	OpPopS:      "pops",
	OpCall:      "call",
	OpIf:        "if",
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

	case OpIf, OpJmp:
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

// Code implements scheme bytecode.
type Code []*Instr

// Print prints the code to standard output.
func (code Code) Print() {
	for _, c := range code {
		fmt.Printf("%s\n", c)
	}
}

// Execute runs the code.
func (scm *Scheme) Execute(code Code) (Value, error) {

	scm.pc = 0
	scm.accu = nil

	frame := scm.pushFrame(nil, true)
	scm.fp = frame.Index

	var err error

	for {
		instr := code[scm.pc]
		scm.pc++

		switch instr.Op {
		case OpConst:
			scm.accu = instr.V

		case OpDefine:
			instr.Sym.Global = scm.accu

		case OpLambda:
			tmpl, ok := instr.V.(*Lambda)
			if !ok {
				return nil, fmt.Errorf("lambda: invalid argument: %V", instr.V)
			}
			lambda := &Lambda{
				MinArgs: tmpl.MinArgs,
				MaxArgs: tmpl.MaxArgs,
				Args:    tmpl.Args,
				Capture: tmpl.Capture,
				Native:  tmpl.Native,
				Code:    tmpl.Code,
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
			scm.accu = instr.Sym.Global

		case OpLocalSet:
			// fmt.Printf("*** local! I=%v, J=%v\n", scm.fp+1+instr.I, instr.J)
			// scm.printStack()
			scm.stack[scm.fp+1+instr.I][instr.J] = scm.accu
			// fmt.Printf(" =>\n")
			// scm.printStack()

		case OpGlobalSet:
			if instr.Sym.Global == nil {
				return nil, fmt.Errorf("undefined symbol '%s'", instr.Sym.Name)
			}
			instr.Sym.Global = scm.accu

		case OpPushF:
			// i.I != 0 for toplevel frames.
			lambda, ok := scm.accu.(*Lambda)
			if !ok {
				return nil, fmt.Errorf("invalid function: %v", scm.accu)
			}
			scm.pushFrame(lambda, instr.I != 0)

		case OpPushS:
			scm.pushScope(instr.I)

		case OpPushA:
			var scope []Value
			err = Map(func(idx int, v Value) error {
				scope = append(scope, v)
				return nil
			}, scm.accu)
			if err != nil {
				return nil, fmt.Errorf("pusha: invalid arguments: %v", err)
			}
			scm.stack = append(scm.stack, scope)

		case OpCall:
			stackTop := len(scm.stack) - 1
			args := scm.stack[stackTop]

			callFrame, ok := scm.stack[stackTop-1][0].(*Frame)
			if !ok || callFrame.Lambda == nil {
				return nil, fmt.Errorf("invalid function: %v", scm.accu)
			}
			lambda := callFrame.Lambda

			if len(args) < lambda.MinArgs {
				return nil, fmt.Errorf("too few arguments: got %v, need %v",
					len(args), lambda.MinArgs)
			}
			if len(args) > lambda.MaxArgs {
				return nil, fmt.Errorf("too many arguments: got %v, max %v",
					len(args), lambda.MaxArgs)
			}

			// Set fp for the call.
			scm.fp = stackTop - 1

			if lambda.Native != nil {
				scm.accu, err = callFrame.Lambda.Native(scm, lambda, args)
				if err != nil {
					return nil, err
				}
				scm.popFrame()
			} else {
				if instr.I != 0 {
					next := callFrame.Next

					nextFrame, ok := scm.stack[next][0].(*Frame)
					if !ok {
						panic(fmt.Sprintf("invalid next frame: %v",
							scm.stack[callFrame.Next]))
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

				// Save current excursion.
				callFrame.PC = scm.pc
				callFrame.Code = code

				code = lambda.Code
				scm.pc = 0
			}

		case OpIf:
			if True(scm.accu) {
				scm.pc += instr.I
			}

		case OpJmp:
			scm.pc += instr.I

		case OpReturn:
			frame, ok := scm.stack[scm.fp][0].(*Frame)
			if !ok {
				return nil, fmt.Errorf("invalid function: %v", scm.accu)
			}
			scm.pc = frame.PC
			code = frame.Code
			scm.popFrame()

		case OpHalt:
			scm.popFrame()
			return scm.accu, nil

		default:
			return nil, fmt.Errorf("%s: not implemented", instr.Op)
		}
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
	PC     int
	Code   Code
}

// Scheme returns the value as a Scheme string.
func (f *Frame) Scheme() string {
	return f.String()
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

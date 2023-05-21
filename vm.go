//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"strings"

	"github.com/markkurossi/scheme/types"
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
	OpEnv
	OpGlobal
	OpLocalSet
	OpEnvSet
	OpGlobalSet
	OpPushF
	OpPushS
	OpPushE
	OpPopS
	OpPopE
	OpPushA
	OpCall
	OpIf
	OpIfNot
	OpJmp
	OpReturn
	OpPairp
	OpCons
	OpCar
	OpCdr
	OpNullp
	OpZerop
	OpNot
	OpAdd
	OpAddI64
	OpSub
	OpSubI64
	OpEq
	OpLt
	OpGt
	OpLe
	OpGe
)

var operands = map[Operand]string{
	OpConst:     "const",
	OpDefine:    "define",
	OpLambda:    "lambda",
	OpLabel:     "label",
	OpLocal:     "local",
	OpEnv:       "env",
	OpGlobal:    "global",
	OpLocalSet:  "local!",
	OpEnvSet:    "env!",
	OpGlobalSet: "global!",
	OpPushF:     "pushf",
	OpPushS:     "pushs",
	OpPushE:     "pushe",
	OpPopS:      "pops",
	OpPopE:      "pope",
	OpPushA:     "pusha",
	OpCall:      "call",
	OpIf:        "if",
	OpIfNot:     "ifnot",
	OpJmp:       "jmp",
	OpReturn:    "return",
	OpPairp:     "pair?",
	OpCons:      "cons",
	OpCar:       "car",
	OpCdr:       "cdr",
	OpNullp:     "null?",
	OpZerop:     "zero?",
	OpNot:       "not",
	OpAdd:       "+",
	OpAddI64:    "+<int64>",
	OpSub:       "-",
	OpSubI64:    "-<int64>",
	OpEq:        "=",
	OpLt:        "<",
	OpGt:        ">",
	OpLe:        "<=",
	OpGe:        ">=",
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

	case OpPushS, OpPushE:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.I)

	case OpLambda:
		return fmt.Sprintf("\t%s\tl%v:%v", i.Op, i.I, i.J)

	case OpLocal, OpLocalSet:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.I)

	case OpEnv, OpEnvSet:
		return fmt.Sprintf("\t%s\t%v.%v", i.Op, i.I, i.J)

	case OpGlobal, OpGlobalSet, OpDefine:
		str := fmt.Sprintf("\t%s\t%v", i.Op, i.Sym)
		if i.I != 0 {
			str += fmt.Sprintf("\t%v", Flags(i.I))
		}
		return str

	case OpIf, OpIfNot, OpJmp:
		return fmt.Sprintf("\t%s\t%v\t; l%v", i.Op, i.I, i.J)

	case OpPopS:
		return fmt.Sprintf("\t%s\t%v", i.Op, i.I)

	case OpCall:
		var suffix string
		if i.I != 0 {
			suffix += "t"
		}
		return fmt.Sprintf("\t%s%s", i.Op, suffix)

	default:
		return fmt.Sprintf("\t%s", i.Op.String())
	}
}

// Apply applies lambda for arguments.
func (scm *Scheme) Apply(lambda Value, args []Value) (Value, error) {
	var argsList, tail Pair

	for _, arg := range args {
		item := NewPair(arg, nil)

		if argsList == nil {
			argsList = item
		} else {
			tail.SetCdr(item)
		}
		tail = item
	}

	var err error
	var env *VMEnvFrame
	var accu Value = lambda

	code := []*Instr{
		{
			Op: OpPushF,
			I:  1,
		},
		{
			Op: OpConst,
			V:  argsList,
		},
		{
			Op: OpPushA,
		},
		{
			Op: OpCall,
		},
	}
	scm.pc = 0
	scm.fp = 0
	scm.sp = 0

	for {
		instr := code[scm.pc]
		scm.pc++

		switch instr.Op {
		case OpConst:
			accu = instr.V

		case OpDefine:
			if instr.Sym.Flags&FlagConst != 0 {
				return nil, scm.Breakf("redefining final symbol '%s'",
					instr.Sym.Name)
			}
			if instr.Sym.Flags&FlagDefined != 0 && !scm.Params.NoWarnDefine {
				scm.VMWarningf("redefining symbol '%s'", instr.Sym.Name)
			}
			instr.Sym.Global = accu
			instr.Sym.Flags |= FlagDefined
			instr.Sym.Flags |= Flags(instr.I)

		case OpLambda:
			tmpl, ok := instr.V.(*LambdaImpl)
			if !ok {
				return nil, scm.Breakf("lambda: invalid argument: %v", instr.V)
			}
			accu = &Lambda{
				Capture: env,
				Impl:    tmpl,
			}

		case OpLabel:

		case OpLocal:
			// fmt.Printf("*** local: %v.%v\n", scm.fp+1+instr.I, instr.J)
			// scm.printStack()
			accu = scm.stack[scm.fp+1+instr.I]

		case OpEnv:
			var e *VMEnvFrame
			for e = env; e != nil; e = e.Next {
				if e.Index == instr.I {
					accu = e.Values[instr.J]
					break
				}
			}
			if e == nil {
				return nil, scm.Breakf("invalid env frame %v", instr.I)
			}

		case OpGlobal:
			if instr.Sym.Flags&FlagDefined == 0 {
				return nil, scm.Breakf("undefined symbol '%s' ", instr.Sym.Name)
			}
			accu = instr.Sym.Global

		case OpLocalSet:
			scm.stack[scm.fp+1+instr.I] = accu

		case OpEnvSet:
			var e *VMEnvFrame
			for e = env; e != nil; e = e.Next {
				if e.Index == instr.I {
					e.Values[instr.J] = accu
					break
				}
			}
			if e == nil {
				return nil, scm.Breakf("invalid env frame %v", instr.I)
			}

		case OpGlobalSet:
			if instr.Sym.Flags&FlagConst != 0 {
				return nil, scm.Breakf("setting final symbol '%s'",
					instr.Sym.Name)
			}
			if instr.Sym.Flags&FlagDefined == 0 {
				return nil, scm.Breakf("undefined symbol '%s'", instr.Sym.Name)
			}
			instr.Sym.Global = accu

		case OpPushF:
			// i.I != 0 for toplevel frames.
			lambda, ok := accu.(*Lambda)
			if !ok {
				return nil, scm.Breakf("%s: invalid function: %v(%T)",
					instr.Op, accu, accu)
			}

			var frame *Frame
			if scm.frameFL != nil {
				frame = scm.frameFL
				scm.frameFL = frame.flNext
			} else {
				frame = new(Frame)
			}

			frame.Index = len(scm.stack)
			frame.Next = scm.fp
			frame.Toplevel = instr.I != 0
			frame.Lambda = lambda

			scm.stack[scm.sp] = frame
			scm.sp++

		case OpPushS:
			for i := 0; i < instr.I; i++ {
				scm.stack[scm.sp] = nil
				scm.sp++
			}

		case OpPushE:
			var index int
			if env != nil {
				index = env.Index + 1
			}
			env = &VMEnvFrame{
				Next:   env,
				Index:  index,
				Values: make([]Value, instr.I),
			}

		case OpPopS:
			scm.sp -= instr.I

		case OpPopE:
			env = env.Next

		case OpPushA:
			var count int
			err = Map(func(idx int, v Value) error {
				scm.stack[scm.sp] = v
				scm.sp++
				count++
				return nil
			}, accu)
			if err != nil {
				return nil, scm.Breakf("pusha: invalid arguments: %v", err)
			}
			accu = Int(count)

		case OpCall:
			vi, ok := accu.(Int)
			if !ok {
				return nil, scm.Breakf("%s: invalid #args: %v", instr.Op, accu)
			}
			numArgs := int(vi)
			args := scm.stack[scm.sp-numArgs : scm.sp]

			callFrame, ok := scm.stack[scm.sp-numArgs-1].(*Frame)
			if !ok || callFrame.Lambda == nil {
				return nil, scm.Breakf("%s: invalid function: %v",
					instr.Op, scm.stack[scm.sp-numArgs-1])
			}
			lambda := callFrame.Lambda

			if numArgs < lambda.Impl.Args.Min {
				return nil, scm.Breakf("too few arguments: got %v, need %v",
					numArgs, lambda.Impl.Args.Min)
			}
			if numArgs > lambda.Impl.Args.Max {
				return nil, scm.Breakf("too many arguments: got %v, max %v",
					numArgs, lambda.Impl.Args.Max)
			}

			// Set fp for the call.
			scm.fp = scm.sp - numArgs - 1

			if lambda.Impl.Native != nil {
				accu, err = callFrame.Lambda.Impl.Native(scm, args)
				if err != nil {
					if len(lambda.Impl.Name) != 0 {
						return nil, scm.Breakf("%s: %v", lambda.Impl.Name, err)
					}
					return nil, scm.Breakf("%v", err)
				}
				scm.sp = scm.fp
				scm.fp = callFrame.Next

				callFrame.flNext = scm.frameFL
				scm.frameFL = callFrame

				continue
			}

			// Save current excursion.
			callFrame.PC = scm.pc
			callFrame.Code = code
			callFrame.Env = env

			// Handle rest arguments.
			if lambda.Impl.Args.Rest != nil {
				var rest Pair
				for i := numArgs - 1; i >= lambda.Impl.Args.Min; i-- {
					scm.sp--
					rest = NewPair(scm.stack[scm.sp], rest)
				}
				scm.stack[scm.sp] = rest
				scm.sp++
				numArgs = lambda.Impl.Args.Min + 1
				args = scm.stack[scm.sp-numArgs : scm.sp]
			}

			env = lambda.Capture

			if lambda.Impl.Captures {
				var index int
				if env != nil {
					index = env.Index + 1
				}
				env = &VMEnvFrame{
					Next:   env,
					Index:  index,
					Values: make([]Value, len(args)),
				}
				copy(env.Values, args)
				scm.sp -= len(args)
			}

			if instr.I != 0 {
				// Tail-call.
				next := callFrame.Next
				nextFrame, ok := scm.stack[next].(*Frame)
				if !ok {
					return nil, scm.Breakf("invalid next frame: %v",
						scm.stack[callFrame.Next])
				}
				nextFrame.Lambda = callFrame.Lambda

				callFrame.flNext = scm.frameFL
				scm.frameFL = callFrame

				count := scm.sp - scm.fp - 1
				copy(scm.stack[next+1:], scm.stack[scm.fp+1:scm.fp+1+count])
				scm.sp = next + 1 + count
				scm.fp = next
			}
			if scm.sp+lambda.Impl.MaxStack > len(scm.stack) {
				return nil, scm.Breakf("out of stack: need %d, got %d",
					lambda.Impl.MaxStack, len(scm.stack)-scm.sp)
			}

			// Jump to lambda code.
			code = lambda.Impl.Code
			scm.pc = 0

		case OpIf:
			if IsTrue(accu) {
				scm.pc += instr.I
			}

		case OpIfNot:
			if !IsTrue(accu) {
				scm.pc += instr.I
			}

		case OpJmp:
			scm.pc += instr.I

		case OpReturn:
			frame, ok := scm.stack[scm.fp].(*Frame)
			if !ok {
				return nil, scm.Breakf("%s: invalid function: %v",
					instr.Op, scm.stack[scm.fp])
			}
			scm.pc = frame.PC
			code = frame.Code
			env = frame.Env

			if scm.popFrame() {
				return accu, nil
			}

		case OpPairp:
			_, ok := accu.(Pair)
			accu = Boolean(ok)

		case OpCons:
			accu = NewPair(scm.stack[scm.sp-2], scm.stack[scm.sp-1])

		case OpCar:
			pair, ok := accu.(Pair)
			if !ok {
				return nil, scm.Breakf("%s: not a pair: %v", instr.Op, accu)
			}
			accu = pair.Car()

		case OpCdr:
			pair, ok := accu.(Pair)
			if !ok {
				return nil, scm.Breakf("%s: not a pair: %v", instr.Op, accu)
			}
			accu = pair.Cdr()

		case OpNullp:
			accu = Boolean(accu == nil)

		case OpZerop:
			accu, err = zero(accu)
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpNot:
			accu = Boolean(!IsTrue(accu))

		case OpAdd:
			accu, err = numAdd(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpAddI64:
			accu = scm.stack[scm.sp-2].(Int) + scm.stack[scm.sp-1].(Int)

		case OpSub:
			accu, err = numSub(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpSubI64:
			accu = scm.stack[scm.sp-2].(Int) - scm.stack[scm.sp-1].(Int)

		case OpEq:
			accu, err = numEq(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpLt:
			accu, err = numLt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpGt:
			accu, err = numGt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}

		case OpLe:
			accu, err = numGt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}
			accu = Boolean(!IsTrue(accu))

		case OpGe:
			accu, err = numLt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				return nil, scm.Breakf("%s: %v", instr.Op, err.Error())
			}
			accu = Boolean(!IsTrue(accu))

		default:
			return nil, scm.Breakf("%s: not implemented", instr.Op)
		}
	}
}

// Breakf breaks the program execution with the error.
func (scm *Scheme) Breakf(format string, a ...interface{}) error {
	err := scm.VMErrorf(format, a...)
	msg := err.Error()

	var compilerError bool

	idx := strings.Index(msg, "<<")
	if idx >= 0 {
		msg = msg[idx+2:]
		compilerError = true
	}
	if !scm.Params.Quiet && !compilerError {
		fmt.Printf("%s\n", msg)
		scm.PrintStack()
	}

	scm.popToplevel()

	return errors.New(msg)
}

// Location returns the source file location of the current VM
// continuation.
func (scm *Scheme) Location() (source string, line int, err error) {
	fp := scm.fp
	pc := scm.pc

	for fp < len(scm.stack) {
		frame, ok := scm.stack[fp].(*Frame)
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

// VMWarningf prints a virtual machine warning.
func (scm *Scheme) VMWarningf(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)

	source, line, err := scm.Location()
	if err != nil || line == 0 || len(source) == 0 {
		fmt.Printf("warning: %v\n", msg)
	} else if line == 0 {
		fmt.Printf("%s: warning: %v\n", source, msg)
	} else {
		fmt.Printf("%s:%v: %s\n", source, line, msg)
	}
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

func (scm *Scheme) popToplevel() {
	fp := scm.fp

	for fp < len(scm.stack) {
		frame, ok := scm.stack[fp].(*Frame)
		if !ok {
			panic("corrupted stack")
		}
		if frame.Toplevel {
			scm.sp = fp
			scm.fp = frame.Next
			break
		}
		if frame.Next == fp {
			break
		}
		fp = frame.Next
	}
}

// PrintStack prints the virtual machine stack.
func (scm *Scheme) PrintStack() {
	fp := scm.fp
	pc := scm.pc

	for fp < len(scm.stack) {
		frame, ok := scm.stack[fp].(*Frame)
		if !ok {
			panic("corrupted stack")
		}

		fmt.Print("  ")
		if frame.Toplevel {
			fmt.Print("\u2514\u2574")
		} else {
			fmt.Print("\u251c\u2574")
		}
		if frame.Lambda != nil {
			fmt.Printf("%v", frame.Lambda.Impl.Signature(false))
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

// StackFrame provides information about the virtual machine stack
// frame.
type StackFrame struct {
	Source string
	Line   int
}

// StackTrace returns information about the virtual machine stack.
func (scm *Scheme) StackTrace() []StackFrame {
	fp := scm.fp
	pc := scm.pc

	var result []StackFrame

	for {
		frame, ok := scm.stack[fp].(*Frame)
		if !ok {
			panic("corrupted stack")
		}

		source, line := frame.MapPC(pc)
		if line > 0 {
			result = append(result, StackFrame{
				Source: source,
				Line:   line,
			})
		}

		if frame.Next == fp {
			break
		}
		fp = frame.Next
		pc = frame.PC
	}
	return result
}

// Intern interns the name and returns the interned symbol.
func (scm *Scheme) Intern(name string) *Identifier {
	id, ok := scm.symbols[name]
	if !ok {
		id = &Identifier{
			Name:       name,
			GlobalType: types.Unspecified,
		}
		scm.symbols[name] = id
	}
	return id
}

func (scm *Scheme) popFrame() bool {
	frame := scm.stack[scm.fp].(*Frame)

	scm.sp = scm.fp
	scm.fp = frame.Next

	frame.flNext = scm.frameFL
	scm.frameFL = frame

	return frame.Toplevel
}

// Frame implements a SCM call stack frame.
type Frame struct {
	Index    int
	Next     int
	Toplevel bool

	Lambda *Lambda
	PC     int
	Code   Code
	Env    *VMEnvFrame
	flNext *Frame
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

// Type implements Value.Type.
func (f *Frame) Type() *types.Type {
	return types.Unspecified
}

// VMEnvFrame implement a virtual machine environment frame.
type VMEnvFrame struct {
	Next   *VMEnvFrame
	Index  int
	Values []Value
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
	scm.printStackLimit(len(scm.stack))
}

func (scm *Scheme) printStackLimit(limit int) {
	var all bool
	if limit > scm.sp {
		limit = 0
		all = true
	} else {
		limit = scm.sp - limit
	}
	fmt.Printf("Stack:\u252c\u2574limit=%v\n", len(scm.stack)-limit)
	for i := scm.sp - 1; i >= limit; i-- {
		if scm.fp == i {
			istr := fmt.Sprintf("%d", i)
			// |123456 []
			fmt.Print("\u251c")
			for j := 0; j < 5-len(istr); j++ {
				fmt.Print("\u2500")
			}
			fmt.Printf(">%s %v\n", istr, scm.stack[i])
		} else {
			fmt.Printf("\u2502%6d %v\n", i, scm.stack[i])
		}
	}
	if all {
		fmt.Printf("\u2570\u2500\u2500\u2500\u2500\u2500\u256f\n")
	} else {
		fmt.Printf("\u2575.....\u2575\n")
	}
}

var vmBuiltins = []Builtin{
	{
		Name:   "error",
		Args:   []string{"who", "message", "irritant..."},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			message, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid message: %v", args[1])
			}
			return nil, fmt.Errorf("%v: %v", args[0], message)
		},
	},
	{
		Name:   "scheme::->scheme",
		Args:   []string{"obj"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return String(ToScheme(args[0])), nil
		},
	},
}

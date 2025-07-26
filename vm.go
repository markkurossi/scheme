//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"math/big"
	"strings"

	"github.com/markkurossi/scheme/types"
)

var (
	_ error = &Error{}
)

// Error implements errors as Values.
type Error struct {
	loc       string
	who       Value
	msg       string
	irritants []Value
}

// NewError creates a new Scheme error from the argument error.
func NewError(err error) *Error {
	e, ok := err.(*Error)
	if ok {
		return e
	}
	return &Error{
		msg: err.Error(),
	}
}

// SetLocation sets the error's source code location.
func (v *Error) SetLocation(source string, line int) {
	if len(source) == 0 && line == 0 {
		return
	}
	v.loc = source
	if line > 0 {
		v.loc += fmt.Sprintf(":%v", line)
	}
}

// Location returns the error's source code location.
func (v *Error) Location() string {
	return v.loc
}

// Error implements Error.Error()
func (v *Error) Error() string {
	var msg string
	if len(v.loc) > 0 {
		msg = v.loc + ": "
	}
	if v.who != nil {
		msg += ToString(v.who)
		msg += ": "
	}
	msg += v.msg
	if len(v.irritants) == 0 {
		return msg
	}
	msg += ": "

	for idx, irritant := range v.irritants {
		if idx > 0 {
			msg += ", "
		}
		msg += ToString(irritant)
	}

	return msg
}

// Scheme implements Value.Scheme.
func (v *Error) Scheme() string {
	return v.msg
}

// Eq implements Value.Eq.
func (v *Error) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal implements Value.Equal.
func (v *Error) Equal(o Value) bool {
	ov, ok := o.(*Error)
	return ok && v.msg == ov.msg
}

// Type implements Value.Type.
func (v *Error) Type() *types.Type {
	return types.Error
}

// Unbox implements Value.Unbox.
func (v *Error) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

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
	OpAddConst
	OpSub
	OpSubI64
	OpSubConst
	OpMul
	OpMulConst
	OpDiv
	OpEq
	OpLt
	OpGt
	OpLe
	OpGe
)

var errGuard = errors.New("guard")

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
	OpAddConst:  "+const",
	OpSub:       "-",
	OpSubI64:    "-<int64>",
	OpSubConst:  "-const",
	OpMul:       "*",
	OpMulConst:  "*const",
	OpDiv:       "/",
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
	Sym *Symbol
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

	case OpLocal, OpLocalSet, OpAddConst, OpSubConst, OpMulConst:
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

// Apply applies lambda for arguments. This function implements the
// virtual machine program execution.
//
// The virtual machine is a stack machine with the following registers:
//   - env - the current environment
//   - fp - the current stack frame
//   - pc - program counter
//   - accu - holds the value of the latest operation
//
// The virtual machine stack is as follows:
//
//	sp --->
//	        Value{n}
//	        ...
//	        Value{0}
//	        Arg{n}
//	        ...
//	        Arg{0}
//	fp ---> Next fp ---+
//	                   |
//	                   v
func (scm *Scheme) Apply(lambda Value, args []Value) (Value, error) {
	var argsList ListBuilder

	for _, arg := range args {
		argsList.Add(arg)
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
			V:  argsList.Head,
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
				err = scm.VMErrorf("redefining final symbol '%s'",
					instr.Sym.Name)
				goto errorHandler
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
				err = scm.VMErrorf("lambda: invalid argument: %v", instr.V)
				goto errorHandler
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
				err = scm.VMErrorf("invalid env frame %v", instr.I)
				goto errorHandler
			}

		case OpGlobal:
			if instr.Sym.Flags&FlagDefined == 0 {
				err = scm.VMErrorf("undefined symbol '%s' ", instr.Sym.Name)
				goto errorHandler
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
				err = scm.VMErrorf("invalid env frame %v", instr.I)
				goto errorHandler
			}

		case OpGlobalSet:
			if instr.Sym.Flags&FlagConst != 0 {
				err = scm.VMErrorf("setting final symbol '%s'", instr.Sym.Name)
				goto errorHandler
			}
			if instr.Sym.Flags&FlagDefined == 0 {
				err = scm.VMErrorf("undefined symbol '%s'", instr.Sym.Name)
				goto errorHandler
			}
			instr.Sym.Global = accu

		case OpPushF:
			// i.I != 0 for toplevel frames.
			lambda, ok := accu.(*Lambda)
			if !ok {
				err = scm.VMErrorf("invalid function: %v", accu)
				goto errorHandler
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
			frame.Tail = 0
			frame.Handler = nil
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
				err = scm.VMErrorf("pusha: invalid arguments: %v", err)
				goto errorHandler
			}
			accu = Int(count)

		case OpCall:
			vi, ok := accu.(Int)
			if !ok {
				err = scm.VMErrorf("%s: invalid #args: %v", instr.Op, accu)
				goto errorHandler
			}
			numArgs := int(vi)
			args := scm.stack[scm.sp-numArgs : scm.sp]

			callFrame, ok := scm.stack[scm.sp-numArgs-1].(*Frame)
			if !ok || callFrame.Lambda == nil {
				err = scm.VMErrorf("%s: invalid function: %v",
					instr.Op, scm.stack[scm.sp-numArgs-1])
				goto errorHandler
			}
			lambda := callFrame.Lambda

			if numArgs < lambda.Impl.Args.Min {
				err = scm.VMErrorf("too few arguments: got %v, need %v",
					numArgs, lambda.Impl.Args.Min)
				goto errorHandler
			}
			if numArgs > lambda.Impl.Args.Max {
				err = scm.VMErrorf("too many arguments: got %v, max %v",
					numArgs, lambda.Impl.Args.Max)
				goto errorHandler
			}

			// Set fp for the call.
			scm.fp = scm.sp - numArgs - 1

		call:
			if lambda.Impl.Native != nil {
				accu, err = callFrame.Lambda.Impl.Native(scm, args)
				if err != nil {
					if err == errGuard {
						// A call with-exception-handler.
						handler, ok := args[0].(*Lambda)
						if !ok {
							err = scm.VMErrorf("invalid handler: %v", args[0])
							goto errorHandler
						}
						lambda, ok = args[1].(*Lambda)
						if !ok {
							err = scm.VMErrorf("invalid thunk: %v", args[1])
							goto errorHandler
						}

						// Clear stack.
						scm.sp = scm.fp + 1

						// Make with-exception-handler call frame a
						// handler frame for thunk.
						callFrame.Tail = int8(instr.I)
						callFrame.Handler = handler
						callFrame.Lambda = lambda

						// Thunk takes no arguments.
						numArgs = 0
						args = nil

						goto call
					}
					schemeErr, ok := err.(*Error)
					if ok {
						// Error from the `error' precedure.
						source, line, err2 := scm.Location()
						if err2 == nil {
							schemeErr.SetLocation(source, line)
						}
					} else {
						if len(lambda.Impl.Name) != 0 {
							err = scm.VMErrorf("%s: %v", lambda.Impl.Name, err)
						} else {
							err = scm.VMErrorf("%v", err)
						}
					}
					goto errorHandler
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
					err = scm.VMErrorf("invalid next frame: %v",
						scm.stack[callFrame.Next])
					goto errorHandler
				}
				nextFrame.Lambda = callFrame.Lambda
				// Copy exception handler only if defined in the call
				// frame. Otherwise we could override it from the
				// recycled frame.
				if callFrame.Handler != nil {
					nextFrame.Tail = callFrame.Tail
					nextFrame.Handler = callFrame.Handler
				}

				callFrame.flNext = scm.frameFL
				scm.frameFL = callFrame

				count := scm.sp - scm.fp - 1
				copy(scm.stack[next+1:], scm.stack[scm.fp+1:scm.fp+1+count])
				scm.sp = next + 1 + count
				scm.fp = next
			}
			if scm.sp+lambda.Impl.MaxStack > len(scm.stack) {
				err = scm.VMErrorf("out of stack: need %d, got %d",
					lambda.Impl.MaxStack, len(scm.stack)-scm.sp)
				goto errorHandler
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
				err = scm.VMErrorf("%s: invalid function: %v",
					instr.Op, scm.stack[scm.fp])
				goto errorHandler
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
				err = scm.VMErrorf("%s: not a pair: %v", instr.Op, accu)
				goto errorHandler
			}
			accu = pair.Car()

		case OpCdr:
			pair, ok := accu.(Pair)
			if !ok {
				err = scm.VMErrorf("%s: not a pair: %v", instr.Op, accu)
				goto errorHandler
			}
			accu = pair.Cdr()

		case OpNullp:
			accu = Boolean(accu == nil)

		case OpZerop:
			accu, err = zero(accu)
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpNot:
			accu = Boolean(!IsTrue(accu))

		case OpAdd:
			accu, err = numAdd(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpAddI64:
			accu = scm.stack[scm.sp-2].(Int) + scm.stack[scm.sp-1].(Int)

		case OpAddConst:
			switch av := accu.(type) {
			case Int:
				accu = av + Int(instr.I)

			case Float:
				accu = av + Float(instr.I)

			case *BigInt:
				accu = &BigInt{
					I: new(big.Int).Add(av.I, big.NewInt(int64(instr.I))),
				}

			case *BigFloat:
				accu = &BigFloat{
					F: new(big.Float).Add(av.F, big.NewFloat(float64(instr.I))),
				}

			default:
				err = scm.VMErrorf("%s: invalid number %v", instr.Op, accu)
				goto errorHandler
			}

		case OpSub:
			accu, err = numSub(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpSubI64:
			accu = scm.stack[scm.sp-2].(Int) - scm.stack[scm.sp-1].(Int)

		case OpSubConst:
			switch av := accu.(type) {
			case Int:
				accu = av - Int(instr.I)

			case Float:
				accu = av - Float(instr.I)

			case *BigInt:
				accu = &BigInt{
					I: new(big.Int).Sub(av.I, big.NewInt(int64(instr.I))),
				}

			case *BigFloat:
				accu = &BigFloat{
					F: new(big.Float).Sub(av.F, big.NewFloat(float64(instr.I))),
				}

			default:
				err = scm.VMErrorf("%s: invalid number %v", instr.Op, accu)
				goto errorHandler
			}

		case OpMul:
			accu, err = numMul(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpMulConst:
			switch av := accu.(type) {
			case Int:
				accu = av * Int(instr.I)

			case Float:
				accu = av * Float(instr.I)

			case *BigInt:
				accu = &BigInt{
					I: new(big.Int).Mul(av.I, big.NewInt(int64(instr.I))),
				}

			case *BigFloat:
				accu = &BigFloat{
					F: new(big.Float).Mul(av.F, big.NewFloat(float64(instr.I))),
				}

			default:
				err = scm.VMErrorf("%s: invalid number %v", instr.Op, accu)
				goto errorHandler
			}

		case OpDiv:
			accu, err = numDiv(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpEq:
			accu, err = numEq(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpLt:
			accu, err = numLt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpGt:
			accu, err = numGt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}

		case OpLe:
			accu, err = numGt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}
			accu = Boolean(!IsTrue(accu))

		case OpGe:
			accu, err = numLt(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				err = scm.VMErrorf("%s: %v", instr.Op, err.Error())
				goto errorHandler
			}
			accu = Boolean(!IsTrue(accu))

		default:
			err = scm.VMErrorf("%s: not implemented", instr.Op)
			goto errorHandler
		}
		continue

	errorHandler:
		f := scm.popToplevel()
		if f == nil || f.Handler == nil {
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
			return nil, errors.New(msg)
		}

		// Restore thunk frame as non-toplevel frame.

		accu = f.Handler
		f.Handler = nil
		f.Toplevel = false

		scm.stack[scm.sp] = f
		scm.fp = scm.sp
		scm.sp++

		// Call handler with error argument.

		// 4		global	handler
		// 5		pushf	false
		// 6		pushs	1
		// 7		const	err
		// 8		local!	1
		// 9		const	1
		// 10		call{t}			// tail call if guard was a tail call
		// 11		return
		code = []*Instr{
			{
				Op: OpPushF,
			},
			{
				Op: OpPushS,
				I:  1,
			},
			{
				Op: OpConst,
				V:  NewError(err),
			},
			{
				Op: OpLocalSet,
				I:  1,
			},
			{
				Op: OpConst,
				V:  Int(1),
			},
			{
				Op: OpCall,
				I:  int(f.Tail),
			},
			{
				Op: OpReturn,
			},
		}
		scm.pc = 0
	}
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
		scm.Stderr.Printf("\u22a5 warning: %v\n", msg)
	} else if line == 0 {
		scm.Stderr.Printf("%s: \u22a5 warning: %v\n", source, msg)
	} else {
		scm.Stderr.Printf("%s:%v: \u22a5 warning: %s\n", source, line, msg)
	}
}

// VMErrorf creates a virtual machine error.
func (scm *Scheme) VMErrorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)

	source, line, err := scm.Location()
	if err != nil || len(source) == 0 {
		return errors.New(msg)
	} else if line == 0 {
		return fmt.Errorf("%s: \u22a5 %s", source, msg)
	}
	return fmt.Errorf("%s:%v: \u22a5 %s", source, line, msg)
}

func (scm *Scheme) popToplevel() *Frame {
	fp := scm.fp

	for fp < len(scm.stack) {
		frame, ok := scm.stack[fp].(*Frame)
		if !ok {
			fmt.Printf("fp=%v, scm.stack[fp]=%v[%T]\n",
				fp, scm.stack[fp], scm.stack[fp])
			panic("corrupted stack")
		}
		if frame.Toplevel || frame.Handler != nil {
			scm.sp = fp
			scm.fp = frame.Next
			return frame
		}
		if frame.Next == fp {
			break
		}
		fp = frame.Next
	}
	return nil
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
		source, line := frame.MapPC(pc)
		if line > 0 {
			fmt.Printf(" at %s:%d", source, line)
		}
		fmt.Println()

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
func (scm *Scheme) Intern(name string) *Symbol {
	id, ok := scm.symbols[name]
	if !ok {
		id = &Symbol{
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
	Tail     int8
	Handler  *Lambda

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

// Unbox implements Value.Unbox.
func (f *Frame) Unbox() (Value, *types.Type) {
	return f, f.Type()
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
	result := fmt.Sprintf("frame: next=%v, toplevel=%v, \u03BB=%v",
		f.Next, toplevel, f.Lambda)
	if f.Handler != nil {
		result += fmt.Sprintf(", handler=%v", f.Handler)
	}
	return result
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
		Return: types.None,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			message, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid message: %v", args[1])
			}
			err := &Error{
				who: args[0],
				msg: string(message),
			}
			for i := 2; i < len(args); i++ {
				err.irritants = append(err.irritants, args[i])
			}
			source, line, err2 := scm.Location()
			if err2 == nil {
				err.SetLocation(source, line)
			}
			return nil, err
		},
	},
	{
		Name:   "raise",
		Args:   []string{"err"},
		Return: types.None,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			err, ok := args[0].(*Error)
			if !ok {
				return nil, fmt.Errorf("invalid error: %v", args[0])
			}
			return nil, err
		},
	},
	{
		Name:   "error?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(*Error)
			return Boolean(ok), nil
		},
	},
	{
		Name:   "error-location",
		Args:   []string{"err"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			err, ok := args[0].(*Error)
			if !ok {
				return nil, fmt.Errorf("invalid error: %v", args[0])
			}
			return String(err.loc), nil
		},
	},
	{
		Name:   "error-who",
		Args:   []string{"err"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			err, ok := args[0].(*Error)
			if !ok {
				return nil, fmt.Errorf("invalid error: %v", args[0])
			}
			return err.who, nil
		},
	},
	{
		Name:   "error-message",
		Args:   []string{"err"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			err, ok := args[0].(*Error)
			if !ok {
				return nil, fmt.Errorf("invalid error: %v", args[0])
			}
			return String(err.msg), nil
		},
	},
	{
		Name:   "error-irritants",
		Args:   []string{"err"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			err, ok := args[0].(*Error)
			if !ok {
				return nil, fmt.Errorf("invalid error: %v", args[0])
			}
			var result ListBuilder
			for _, irritant := range err.irritants {
				result.Add(irritant)
			}
			return result.Head, nil
		},
	},
	{
		Name:   "with-exception-handler",
		Args:   []string{"handler<lambda(err)any>", "thunk<lambda()any>"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return nil, errGuard
		},
		Parametrize: func(args []*types.Type) (*types.Type, error) {
			var returnType *types.Type
			for _, t := range args {
				if t.Enum != types.EnumLambda {
					return nil, fmt.Errorf("invalid argument: %v, expected %v",
						t, types.EnumLambda)
				}
				returnType = types.Unify(returnType, t.Return)
			}
			return returnType, nil
		},
	},
	{
		Name:   "scheme::stack-size",
		Return: types.InexactInteger,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return Int(len(scm.stack)), nil
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

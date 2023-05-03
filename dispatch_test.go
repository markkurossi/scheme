//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"testing"
)

var benchmarkDispatchCode = []Instr{
	{
		Op: OpPushS,
		I:  1,
	},
	{
		Op: OpConst,
		V:  Int(100000),
	},
	{
		Op: OpLocalSet,
		I:  0,
	},
	// Loop
	{
		Op: OpLocal,
		I:  0,
	},
	{
		Op: OpZerop,
	},
	{
		Op: OpIf,
		I:  9, // Return
	},
	// Decrement.
	{
		Op: OpPushS,
		I:  2,
	},
	{
		Op: OpLocal,
		I:  0,
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
		Op: OpLocalSet,
		I:  2,
	},
	{
		Op: OpSub,
	},
	{
		Op: OpLocalSet,
		I:  0,
	},
	{
		Op: OpPopS,
		I:  2,
	},
	{
		Op: OpJmp,
		I:  -12,
	},

	// Return
	{
		Op: OpReturn,
	},
}

type instrScheme struct {
	pc    int
	fp    int
	sp    int
	stack []Value
}

func benchmarkInstrScheme(b *testing.B, code []*Instr, scm *instrScheme) {
	var err error
	var accu Value

	scm.pc = 0
	scm.fp = 0
	scm.sp = 1

	for {
		instr := code[scm.pc]
		scm.pc++

		switch instr.Op {

		case OpConst:
			accu = instr.V

		case OpPushS:
			for i := 0; i < instr.I; i++ {
				scm.stack[scm.sp] = nil
				scm.sp++
			}

		case OpPopS:
			scm.sp -= instr.I

		case OpLocal:
			accu = scm.stack[scm.fp+1+instr.I]

		case OpLocalSet:
			scm.stack[scm.fp+1+instr.I] = accu

		case OpIf:
			if IsTrue(accu) {
				scm.pc += instr.I
			}

		case OpJmp:
			scm.pc += instr.I

		case OpZerop:
			vint, ok := accu.(Int)
			accu = Boolean(ok && vint == 0)

		case OpSub:
			accu, err = numSub(scm.stack[scm.sp-2], scm.stack[scm.sp-1])
			if err != nil {
				b.Fatalf("sub: %v", err)
			}

		case OpReturn:
			return

		default:
			b.Fatalf("invalid operand: %v", instr.Op)
		}
	}
}

func BenchmarkInstrScheme(b *testing.B) {
	var code []*Instr
	for _, c := range benchmarkDispatchCode {
		code = append(code, &c)
	}
	scm := &instrScheme{
		stack: make([]Value, 4096),
	}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		benchmarkInstrScheme(b, code, scm)
	}
}

func benchmarkInstrPtr(b *testing.B, code []*Instr, stack []Value) {
	var err error
	var accu Value

	var pc int
	var fp int
	var sp int

	sp++

	for {
		instr := code[pc]
		pc++

		switch instr.Op {

		case OpConst:
			accu = instr.V

		case OpPushS:
			for i := 0; i < instr.I; i++ {
				stack[sp] = nil
				sp++
			}

		case OpPopS:
			sp -= instr.I

		case OpLocal:
			accu = stack[fp+1+instr.I]

		case OpLocalSet:
			stack[fp+1+instr.I] = accu

		case OpIf:
			if IsTrue(accu) {
				pc += instr.I
			}

		case OpJmp:
			pc += instr.I

		case OpZerop:
			vint, ok := accu.(Int)
			accu = Boolean(ok && vint == 0)

		case OpSub:
			accu, err = numSub(stack[sp-2], stack[sp-1])
			if err != nil {
				b.Fatalf("sub: %v", err)
			}

		case OpReturn:
			return

		default:
			b.Fatalf("invalid operand: %v", instr.Op)
		}
	}
}

func BenchmarkInstrPtr(b *testing.B) {
	var code []*Instr
	for _, c := range benchmarkDispatchCode {
		code = append(code, &c)
	}
	stack := make([]Value, 4096)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		benchmarkInstrPtr(b, code, stack)
	}
}

func benchmarkInstr(b *testing.B, code []Instr, stack []Value) {
	var err error
	var accu Value

	var pc int
	var fp int
	var sp int

	sp++

	for {
		instr := &code[pc]
		pc++

		switch instr.Op {

		case OpConst:
			accu = instr.V

		case OpPushS:
			for i := 0; i < instr.I; i++ {
				stack[sp] = nil
				sp++
			}

		case OpPopS:
			sp -= instr.I

		case OpLocal:
			accu = stack[fp+1+instr.I]

		case OpLocalSet:
			stack[fp+1+instr.I] = accu

		case OpIf:
			if IsTrue(accu) {
				pc += instr.I
			}

		case OpJmp:
			pc += instr.I

		case OpZerop:
			vint, ok := accu.(Int)
			accu = Boolean(ok && vint == 0)

		case OpSub:
			accu, err = numSub(stack[sp-2], stack[sp-1])
			if err != nil {
				b.Fatalf("sub: %v", err)
			}

		case OpReturn:
			return

		default:
			b.Fatalf("invalid operand: %v", instr.Op)
		}
	}
}

func BenchmarkInstr(b *testing.B) {
	var code []Instr
	for _, c := range benchmarkDispatchCode {
		code = append(code, c)
	}
	stack := make([]Value, 4096)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		benchmarkInstr(b, code, stack)
	}
}

func benchmarkBC(b *testing.B, code []byte, consts, stack []Value) {
	var err error
	var accu Value

	var pc int
	var fp int
	var sp int

	sp++

	for {
		op := Operand(code[pc])
		pc++

		switch op {

		case OpConst:
			idx := int(code[pc])
			pc++
			accu = consts[idx]

		case OpPushS:
			idx := int(code[pc])
			pc++
			for i := 0; i < idx; i++ {
				stack[sp] = nil
				sp++
			}

		case OpPopS:
			idx := int(code[pc])
			pc++
			sp -= idx

		case OpLocal:
			idx := int(code[pc])
			pc++
			accu = stack[fp+1+idx]

		case OpLocalSet:
			idx := int(code[pc])
			pc++
			stack[fp+1+idx] = accu

		case OpIf:
			idx := int8(code[pc])
			pc++
			if IsTrue(accu) {
				pc += int(idx)
			}

		case OpJmp:
			idx := int8(code[pc])
			pc++
			pc += int(idx)

		case OpZerop:
			vint, ok := accu.(Int)
			accu = Boolean(ok && vint == 0)

		case OpSub:
			accu, err = numSub(stack[sp-2], stack[sp-1])
			if err != nil {
				b.Fatalf("sub: %v", err)
			}

		case OpReturn:
			return

		default:
			b.Fatalf("invalid operand: %v", op)
		}
	}
}

func BenchmarkBC(b *testing.B) {
	var code []byte
	var consts []Value
	var offsets []int

	for _, instr := range benchmarkDispatchCode {
		offsets = append(offsets, len(code))
		code = append(code, byte(instr.Op))

		switch instr.Op {
		case OpConst:
			code = append(code, byte(len(consts)))
			consts = append(consts, instr.V)

		case OpPushS, OpPopS, OpLocal, OpLocalSet:
			code = append(code, byte(instr.I))

		case OpIf, OpJmp:
			code = append(code, 0)
		}
	}
	for pc, instr := range benchmarkDispatchCode {
		switch instr.Op {
		case OpIf, OpJmp:
			target := pc + 1 + instr.I
			targetOffset := offsets[target]
			thisOffset := offsets[pc]
			code[thisOffset+1] = byte(targetOffset - (thisOffset + 2))
		}
	}

	stack := make([]Value, 4096)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		benchmarkBC(b, code, consts, stack)
	}
}

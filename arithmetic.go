//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"math"
	"math/big"
	"strconv"
	"strings"

	"github.com/markkurossi/scheme/types"
)

// Number represents numbers as they appeared in the source code. They
// capture the actual number value and the lexical type of the number.
type Number struct {
	val Value
	t   *types.Type
}

// NewNumber creates a new Number with the value and its lexical type.
func NewNumber(v Value, t *types.Type) *Number {
	return &Number{
		val: v,
		t:   t,
	}
}

func (v *Number) String() string {
	return fmt.Sprintf("%v", v.val)
}

// Scheme implements Value.Scheme.
func (v *Number) Scheme() string {
	if !v.t.IsA(types.Number) {
		return v.val.Scheme()
	}
	switch val := v.val.(type) {
	case Int:
		return fmt.Sprintf("%v", int64(val))
	case Float:
		return fmt.Sprintf("%v", float64(val))
	case *BigInt:
		return val.I.String()
	case *BigFloat:
		return val.F.String()
	default:
		panic(fmt.Sprintf("invalid number: %v(%T)", val, val))
	}
}

// Eq implements Value.Eq.
func (v *Number) Eq(o Value) bool {
	ov, ok := o.(*Number)
	if !ok {
		return false
	}
	return v.val.Eq(ov.val) && v.t.IsA(ov.t)
}

// Equal implements Value.Equal.
func (v *Number) Equal(o Value) bool {
	ov, ok := o.(*Number)
	if ok {
		return v.val.Equal(ov.val)
	}
	return v.val.Equal(o)
}

// Type implements Value.Type.
func (v *Number) Type() *types.Type {
	return v.t
}

// Unbox implements Value.Unbox.
func (v *Number) Unbox() (Value, *types.Type) {
	return Unbox(v.val)
}

// Value returns the native number value.
func (v *Number) Value() Value {
	return v.val
}

// MakeNumber creates a new numeric value from the native number. The
// function panics if the input value type is not supported.
func MakeNumber(value interface{}) Value {
	switch v := value.(type) {
	case int:
		return Int(v)

	case int64:
		return Int(v)

	case float64:
		return Float(v)

	case *big.Int:
		return &BigInt{
			I: v,
		}

	case *big.Float:
		return &BigFloat{
			F: v,
		}

	default:
		panic(fmt.Sprintf("unsupported number: %v(%T)", v, v))
	}
}

// Int implements inexact integer numbers.
type Int int64

func (v Int) String() string {
	return fmt.Sprintf("%v", int64(v))
}

// Scheme implements Value.Scheme.
func (v Int) Scheme() string {
	return v.String()
}

// Eq implements Value.Eq.
func (v Int) Eq(o Value) bool {
	ov, ok := o.(Int)
	if !ok {
		return false
	}
	return v == ov
}

// Equal implements Value.Equal.
func (v Int) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return v == ov

	case Float:
		return float64(v) == float64(ov)

	case *BigInt:
		return ov.I.Cmp(big.NewInt(int64(v))) == 0

	case *BigFloat:
		return ov.F.Cmp(big.NewFloat(float64(v))) == 0

	case *Number:
		return v.Equal(ov.Value())

	default:
		return false
	}
}

// Type implements Value.Type.
func (v Int) Type() *types.Type {
	return types.InexactInteger
}

// Unbox implements Value.Unbox.
func (v Int) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

// Float implements inexact floating point numbers.
type Float float64

func (v Float) String() string {
	return fmt.Sprintf("%v", float64(v))
}

// Scheme implements Value.Scheme.
func (v Float) Scheme() string {
	return v.String()
}

// Eq implements Value.Eq.
func (v Float) Eq(o Value) bool {
	ov, ok := o.(Float)
	if !ok {
		return false
	}
	return v == ov
}

// Equal implements Value.Equal.
func (v Float) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return float64(v) == float64(ov)

	case Float:
		return v == ov

	case *BigInt:
		return new(big.Float).SetInt(ov.I).Cmp(big.NewFloat(float64(v))) == 0

	case *BigFloat:
		return ov.F.Cmp(big.NewFloat(float64(v))) == 0

	case *Number:
		return v.Equal(ov.Value())

	default:
		return false
	}
}

// Type implements Value.Type.
func (v Float) Type() *types.Type {
	return types.InexactFloat
}

// Unbox implements Value.Unbox.
func (v Float) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

// BigInt implements exact integer numbers.
type BigInt struct {
	I *big.Int
}

func (v *BigInt) String() string {
	return v.I.String()
}

// Scheme implements Value.Scheme.
func (v *BigInt) Scheme() string {
	return "#e" + v.String()
}

// Eq implements Value.Eq.
func (v *BigInt) Eq(o Value) bool {
	ov, ok := o.(*BigInt)
	if !ok {
		return false
	}
	return v.I.Cmp(ov.I) == 0
}

// Equal implements Value.Equal.
func (v *BigInt) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return v.I.Cmp(big.NewInt(int64(ov))) == 0

	case Float:
		return new(big.Float).SetInt(v.I).Cmp(big.NewFloat(float64(ov))) == 0

	case *BigInt:
		return v.I.Cmp(ov.I) == 0

	case *BigFloat:
		return new(big.Float).SetInt(v.I).Cmp(ov.F) == 0

	case *Number:
		return v.Equal(ov.Value())

	default:
		return false
	}
}

// Type implements Value.Type.
func (v *BigInt) Type() *types.Type {
	return types.ExactInteger
}

// Unbox implements Value.Unbox.
func (v *BigInt) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

// BigFloat implements exact floating point numbers.
type BigFloat struct {
	F *big.Float
}

func (v *BigFloat) String() string {
	return v.F.String()
}

// Scheme implements Value.Scheme.
func (v *BigFloat) Scheme() string {
	return "#e" + v.String()
}

// Eq implements Value.Eq.
func (v *BigFloat) Eq(o Value) bool {
	ov, ok := o.(*BigFloat)
	if !ok {
		return false
	}
	return v.F.Cmp(ov.F) == 0
}

// Equal implements Value.Equal.
func (v *BigFloat) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return v.F.Cmp(big.NewFloat(float64(ov))) == 0

	case Float:
		return v.F.Cmp(big.NewFloat(float64(ov))) == 0

	case *BigInt:
		return v.F.Cmp(big.NewFloat(0.0).SetInt(ov.I)) == 0

	case *BigFloat:
		return v.F.Cmp(ov.F) == 0

	case *Number:
		return v.Equal(ov.Value())

	default:
		return false
	}
}

// Type implements Value.Type.
func (v *BigFloat) Type() *types.Type {
	return types.ExactFloat
}

// Unbox implements Value.Unbox.
func (v *BigFloat) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

// Int64 returns the number value as int64 integer number. The
// function returns an error if the argument is not a number.
func Int64(v Value) (int64, error) {
	switch val := v.(type) {
	case Int:
		return int64(val), nil

	case Float:
		return int64(val), nil

	case *BigInt:
		return val.I.Int64(), nil

	case *BigFloat:
		v, _ := val.F.Int64()
		return v, nil

	default:
		return 0, fmt.Errorf("invalid number: %v", v)
	}
}

func numAdd(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 + v2, nil

		case Float:
			return Float(v1) + v2, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Add(big.NewInt(int64(v1)), v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Add(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return v1 + Float(v2), nil

		case Float:
			return v1 + v2, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Add(big.NewFloat(float64(v1)),
					new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Add(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Float(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return &BigInt{
				I: new(big.Int).Add(v1.I, big.NewInt(int64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Add(new(big.Float).SetInt(v1.I),
					big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Add(v1.I, v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Add(new(big.Float).SetInt(v1.I), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return &BigFloat{
				F: new(big.Float).Add(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Add(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Add(v1.F, new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Add(v1.F, v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("invalid number: %v", z1)
	}

}

func numSub(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 - v2, nil

		case Float:
			return Float(v1) - v2, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Sub(big.NewInt(int64(v1)), v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Sub(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return v1 - Float(v2), nil

		case Float:
			return v1 - v2, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Sub(big.NewFloat(float64(v1)),
					new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Sub(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Float(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return &BigInt{
				I: new(big.Int).Sub(v1.I, big.NewInt(int64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Sub(new(big.Float).SetInt(v1.I),
					big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Sub(v1.I, v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Sub(new(big.Float).SetInt(v1.I), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return &BigFloat{
				F: new(big.Float).Sub(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Sub(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Sub(v1.F, new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Sub(v1.F, v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("invalid number: %v", z1)
	}
}

func numMul(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 * v2, nil

		case Float:
			return Float(v1) * v2, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Mul(big.NewInt(int64(v1)), v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Mul(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return v1 * Float(v2), nil

		case Float:
			return v1 * v2, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Mul(big.NewFloat(float64(v1)),
					new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Mul(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Float(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return &BigInt{
				I: new(big.Int).Mul(v1.I, big.NewInt(int64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Mul(new(big.Float).SetInt(v1.I),
					big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Mul(v1.I, v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Mul(new(big.Float).SetInt(v1.I), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return &BigFloat{
				F: new(big.Float).Mul(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Mul(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Mul(v1.F, new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Mul(v1.F, v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("invalid number: %v", z1)
	}
}

func numDiv(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 / v2, nil

		case Float:
			return Float(v1) / v2, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Quo(big.NewInt(int64(v1)), v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Quo(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return v1 / Float(v2), nil

		case Float:
			return v1 / v2, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Quo(big.NewFloat(float64(v1)),
					new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Quo(big.NewFloat(float64(v1)), v2.F),
			}, nil

		default:
			return Float(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return &BigInt{
				I: new(big.Int).Quo(v1.I, big.NewInt(int64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Quo(new(big.Float).SetInt(v1.I),
					big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Quo(v1.I, v2.I),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Quo(new(big.Float).SetInt(v1.I), v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return &BigFloat{
				F: new(big.Float).Quo(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case Float:
			return &BigFloat{
				F: new(big.Float).Quo(v1.F, big.NewFloat(float64(v2))),
			}, nil

		case *BigInt:
			return &BigFloat{
				F: new(big.Float).Quo(v1.F, new(big.Float).SetInt(v2.I)),
			}, nil

		case *BigFloat:
			return &BigFloat{
				F: new(big.Float).Quo(v1.F, v2.F),
			}, nil

		default:
			return Int(0), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("invalid number: %v", z1)
	}
}

func numEq(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int, Float, *BigInt, *BigFloat:
		switch v2 := z2.(type) {
		case Int, Float, *BigInt, *BigFloat:
			return Boolean(v1.Equal(v2)), nil
		default:
			return nil, fmt.Errorf("invalid argument: %v", z2)
		}
	default:
		return nil, fmt.Errorf("invalid argument: %v", z1)
	}
}

func numLt(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 < v2), nil

		case Float:
			return Boolean(Float(v1) < v2), nil

		case *BigInt:
			return Boolean(big.NewInt(int64(v1)).Cmp(v2.I) == -1), nil

		case *BigFloat:
			return Boolean(big.NewFloat(float64(v1)).Cmp(v2.F) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 < Float(v2)), nil

		case Float:
			return Boolean(v1 < v2), nil

		case *BigInt:
			return Boolean(big.NewFloat(float64(v1)).
				Cmp(new(big.Float).SetInt(v2.I)) == -1), nil

		case *BigFloat:
			return Boolean(big.NewFloat(float64(v1)).Cmp(v2.F) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.I.Cmp(big.NewInt(int64(v2))) == -1), nil

		case Float:
			return Boolean(new(big.Float).SetInt(v1.I).
				Cmp(big.NewFloat(float64(v2))) == -1), nil

		case *BigInt:
			return Boolean(v1.I.Cmp(v2.I) == -1), nil

		case *BigFloat:
			return Boolean(new(big.Float).SetInt(v1.I).Cmp(v2.F) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.F.Cmp(big.NewFloat(float64(v2))) == -1), nil

		case Float:
			return Boolean(v1.F.Cmp(big.NewFloat(float64(v2))) == -1), nil

		case *BigInt:
			return Boolean(v1.F.Cmp(new(big.Float).SetInt(v2.I)) == -1), nil

		case *BigFloat:
			return Boolean(v1.F.Cmp(v2.F) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Boolean(false), fmt.Errorf("invalid number: %v", z1)
	}
}

func numGt(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 > v2), nil

		case Float:
			return Boolean(Float(v1) > v2), nil

		case *BigInt:
			return Boolean(big.NewInt(int64(v1)).Cmp(v2.I) == 1), nil

		case *BigFloat:
			return Boolean(big.NewFloat(float64(v1)).Cmp(v2.F) == 1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case Float:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 > Float(v2)), nil

		case Float:
			return Boolean(v1 > v2), nil

		case *BigInt:
			return Boolean(big.NewFloat(float64(v1)).
				Cmp(new(big.Float).SetInt(v2.I)) == 1), nil

		case *BigFloat:
			return Boolean(big.NewFloat(float64(v1)).Cmp(v2.F) == 1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.I.Cmp(big.NewInt(int64(v2))) == 1), nil

		case Float:
			return Boolean(new(big.Float).SetInt(v1.I).
				Cmp(big.NewFloat(float64(v2))) == 1), nil

		case *BigInt:
			return Boolean(v1.I.Cmp(v2.I) == 1), nil

		case *BigFloat:
			return Boolean(new(big.Float).SetInt(v1.I).Cmp(v2.F) == 1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	case *BigFloat:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.F.Cmp(big.NewFloat(float64(v2))) == 1), nil

		case Float:
			return Boolean(v1.F.Cmp(big.NewFloat(float64(v2))) == 1), nil

		case *BigInt:
			return Boolean(v1.F.Cmp(new(big.Float).SetInt(v2.I)) == 1), nil

		case *BigFloat:
			return Boolean(v1.F.Cmp(v2.F) == 1), nil

		default:
			return Boolean(false), fmt.Errorf("invalid number: %v", z2)
		}

	default:
		return Boolean(false), fmt.Errorf("invalid number: %v", z1)
	}
}

func zero(z Value) (Value, error) {
	switch v := z.(type) {
	case Int:
		return Boolean(v == 0), nil

	case Float:
		return Boolean(v == 0.0), nil

	case *BigInt:
		return Boolean(v.I.BitLen() == 0), nil

	case *BigFloat:
		f, _ := v.F.Float64()
		return Boolean(f == 0.0), nil

	default:
		return Boolean(false), fmt.Errorf("invalid number: %v", z)
	}
}

func bit(z Value, i int) (uint, error) {
	switch v := z.(type) {
	case Int:
		return uint((v >> i) & 0x1), nil

	case *BigInt:
		return v.I.Bit(i), nil

	default:
		return 0, fmt.Errorf("bit: invalid number: %v", z)
	}
}

func parametrizeNumber(args []*types.Type) (*types.Type, error) {
	var returnType *types.Type

	for _, t := range args {
		returnType = types.Unify(returnType, t)
	}
	if returnType == nil {
		returnType = types.Number
	}
	if returnType.Concrete() && !returnType.IsKindOf(types.Number) {
		return nil, fmt.Errorf("invalid arguments: %v, expected %v",
			returnType, types.Number)
	}
	return returnType, nil
}

var numberBuiltins = []Builtin{
	{
		Name:   "number?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int, Float, *BigInt, *BigFloat, *Number:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX complex?
	// XXX real?
	// XXX rational?
	{
		Name:   "integer?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int, *BigInt:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	{
		Name:   "float?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case Float, *BigFloat:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX real-valued?
	// XXX rational-valued?
	// XXX integer-valued?
	{
		Name:   "exact?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case *BigInt, *BigFloat:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	{
		Name:   "inexact?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int, Float:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX inexact
	// XXX exact
	{
		Name:   "scheme::=",
		Args:   []string{"z1", "z2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			v, err := numEq(args[0], args[1])
			if err != nil {
				return nil, fmt.Errorf("%v", err.Error())
			}
			return v, nil
		},
	},
	{
		Name:   "scheme::<",
		Args:   []string{"x1", "x2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			v, err := numLt(args[0], args[1])
			if err != nil {
				return nil, fmt.Errorf("%v", err.Error())
			}
			return v, nil
		},
	},
	{
		Name:   "scheme::>",
		Args:   []string{"x1", "x2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			v, err := numGt(args[0], args[1])
			if err != nil {
				return nil, fmt.Errorf("%v", err.Error())
			}
			return v, nil
		},
	},
	{
		Name:   "zero?",
		Args:   []string{"z"},
		Return: types.Boolean,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return zero(args[0])
		},
	},
	{
		Name:   "odd?",
		Args:   []string{"n"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			bit, err := bit(args[0], 0)
			if err != nil {
				return nil, fmt.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(bit == 1), nil
		},
	},
	{
		Name:   "even?",
		Args:   []string{"n"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			bit, err := bit(args[0], 0)
			if err != nil {
				return nil, fmt.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(bit == 0), nil
		},
	},
	{
		Name:   "+",
		Args:   []string{"z1..."},
		Return: types.Number,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			if len(args) == 0 {
				return Int(0), nil
			}
			var sum Value
			var err error

			switch v := args[0].(type) {
			case Int, Float, *BigInt, *BigFloat:
				sum = v
			default:
				return Int(0), fmt.Errorf("invalid number: %v", v)
			}
			for i := 1; i < len(args); i++ {
				sum, err = numAdd(sum, args[i])
				if err != nil {
					return sum, fmt.Errorf("%v", err.Error())
				}
			}
			return sum, nil
		},
		Parametrize: parametrizeNumber,
	},
	{
		Name:   "scheme::*",
		Args:   []string{"z1", "z2"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return numMul(args[0], args[1])
		},
		Parametrize: parametrizeNumber,
	},
	{
		Name:   "-",
		Args:   []string{"z1", "z2..."},
		Return: types.Number,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			if len(args) == 1 {
				switch v := args[0].(type) {
				case Int:
					return -v, nil

				case Float:
					return -v, nil

				case *BigInt:
					return numSub(&BigInt{
						I: big.NewInt(0),
					}, v)

				case *BigFloat:
					return numSub(&BigFloat{
						F: big.NewFloat(0.0),
					}, v)

				default:
					return Int(0), fmt.Errorf("invalid number: %v", v)
				}
			}

			var diff Value
			var err error

			switch v := args[0].(type) {
			case Int, Float, *BigInt, *BigFloat:
				diff = v
			default:
				return Int(0), fmt.Errorf("invalid number: %v", v)
			}

			for i := 1; i < len(args); i++ {
				diff, err = numSub(diff, args[i])
				if err != nil {
					return diff, fmt.Errorf("%v", err.Error())
				}
			}

			return diff, nil
		},
		Parametrize: parametrizeNumber,
	},
	{
		Name:   "scheme::/",
		Args:   []string{"z1", "z2"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return numDiv(args[0], args[1])
		},
		Parametrize: parametrizeNumber,
	},
	{
		Name:   "abs",
		Args:   []string{"x"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case Int:
				if v < 0 {
					return Int(-v), nil
				}
				return v, nil

			case Float:
				if v < 0 {
					return Float(-v), nil
				}
				return v, nil

			case *BigInt:
				return &BigInt{
					I: new(big.Int).Abs(v.I),
				}, nil

			case *BigFloat:
				return &BigFloat{
					F: new(big.Float).Abs(v.F),
				}, nil

			default:
				return Int(0), fmt.Errorf("invalid number: %v", args[0])
			}
		},
		Parametrize: parametrizeNumber,
	},
	{
		Name:    "mod",
		Aliases: []string{"modulo"},
		Args:    []string{"z1", "z2"},
		Return:  types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v1 := args[0].(type) {
			case Int:
				switch v2 := args[1].(type) {
				case Int:
					return v1 % v2, nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Mod(big.NewInt(int64(v1)), v2.I),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			case *BigInt:
				switch v2 := args[1].(type) {
				case Int:
					return &BigInt{
						I: new(big.Int).Mod(v1.I, big.NewInt(int64(v2))),
					}, nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Mod(v1.I, v2.I),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			default:
				return Int(0), fmt.Errorf("invalid number: %v", args[0])
			}
		},
	},
	{
		Name:   "sqrt",
		Args:   []string{"z"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case Int:
				return Int(math.Sqrt(float64(v))), nil

			case Float:
				return Int(math.Sqrt(float64(v))), nil

			case *BigInt:
				return &BigInt{
					I: new(big.Int).Sqrt(v.I),
				}, nil

			case *BigFloat:
				return &BigFloat{
					F: new(big.Float).Sqrt(v.F),
				}, nil

			default:
				return Int(0), fmt.Errorf("invalid number: %v", args[0])
			}
		},
	},
	{
		Name:   "expt",
		Args:   []string{"z1", "z2"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v1 := args[0].(type) {
			case Int:
				switch v2 := args[1].(type) {
				case Int:
					return Int(math.Pow(float64(v1), float64(v2))), nil

				case Float:
					return Float(math.Pow(float64(v1), float64(v2))), nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Exp(big.NewInt(int64(v1)), v2.I, nil),
					}, nil

				case *BigFloat:
					f2, _ := v2.F.Int(new(big.Int))
					return &BigInt{
						I: new(big.Int).Exp(big.NewInt(int64(v1)), f2, nil),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			case Float:
				switch v2 := args[1].(type) {
				case Int:
					return Int(math.Pow(float64(v1), float64(v2))), nil

				case Float:
					return Float(math.Pow(float64(v1), float64(v2))), nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Exp(big.NewInt(int64(v1)), v2.I, nil),
					}, nil

				case *BigFloat:
					f2, _ := v2.F.Int(new(big.Int))
					return &BigInt{
						I: new(big.Int).Exp(big.NewInt(int64(v1)), f2, nil),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			case *BigInt:
				switch v2 := args[1].(type) {
				case Int:
					return &BigInt{
						I: new(big.Int).Exp(v1.I, big.NewInt(int64(v2)), nil),
					}, nil

				case Float:
					return &BigInt{
						I: new(big.Int).Exp(v1.I, big.NewInt(int64(v2)), nil),
					}, nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Exp(v1.I, v2.I, nil),
					}, nil

				case *BigFloat:
					f2, _ := v2.F.Int(new(big.Int))
					return &BigInt{
						I: new(big.Int).Exp(v1.I, f2, nil),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			case *BigFloat:
				f1, _ := v1.F.Int(new(big.Int))

				switch v2 := args[1].(type) {
				case Int:
					return &BigInt{
						I: new(big.Int).Exp(f1, big.NewInt(int64(v2)), nil),
					}, nil

				case Float:
					return &BigInt{
						I: new(big.Int).Exp(f1, big.NewInt(int64(v2)), nil),
					}, nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Exp(f1, v2.I, nil),
					}, nil

				case *BigFloat:
					f2, _ := v2.F.Int(new(big.Int))
					return &BigInt{
						I: new(big.Int).Exp(f1, f2, nil),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			default:
				return Int(0), fmt.Errorf("expt: invalid number: %v", args[0])
			}
		},
	},
	{
		Name:   "number->string",
		Args:   []string{"z", "[radix<int>]", "[precision<int>]"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var radix int = 10
			var precision int

			if len(args) > 1 {
				v, err := Int64(args[1])
				if err != nil {
					return nil, fmt.Errorf("invalid radix: %v", args[1])
				}
				radix = int(v)
			}
			switch radix {
			case 2, 8, 10, 16:

			default:
				return nil, fmt.Errorf("invalid radix %v: expected %v",
					args[1], "2, 8, 10, or 16")
			}

			if len(args) > 2 {
				v, err := Int64(args[2])
				if err != nil {
					return nil, fmt.Errorf("invalid precision: %v", args[2])
				}
				precision = int(v)
			}
			_ = precision

			val, _ := Unbox(args[0])

			switch v := val.(type) {
			case Int:
				return String(strconv.FormatInt(int64(v), radix)), nil

			case Float:
				return String(strconv.FormatFloat(float64(v), 'g', -1, 64)), nil

			case *BigInt:
				return String(v.I.Text(radix)), nil

			case *BigFloat:
				return String(v.F.Text('g', -1)), nil

			default:
				return nil, fmt.Errorf("invalid number: %v", args[0])
			}
		},
	},
	{
		Name:   "string->number",
		Args:   []string{"string", "[radix<int>]"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var radix int

			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}

			if len(args) > 1 {
				v, err := Int64(args[1])
				if err != nil {
					return nil, fmt.Errorf("invalid radix: %v", args[1])
				}
				radix = int(v)

				var prefix string
				switch radix {
				case 2:
					prefix = "#b"
				case 8:
					prefix = "#o"
				case 10:
					prefix = "#d"
				case 16:
					prefix = "#x"

				default:
					return nil, fmt.Errorf("invalid radix %v: expected %v",
						args[1], "2, 8, 10, or 16")
				}
				if str[0] != '#' {
					str = String(prefix) + str
				}
			}

			parser := NewSexprParser("{data}", strings.NewReader(string(str)))
			v, err := parser.Next()
			if err != nil {
				return Boolean(false), nil
			}
			_, err = parser.Next()
			if err == nil || err != io.EOF {
				return Boolean(false), nil
			}
			v, _ = Unbox(v)

			switch v.(type) {
			case Int, *BigInt:
				return v, nil

			default:
				return Boolean(false), nil
			}
		},
	},
	{
		Name:   "number->float",
		Args:   []string{"x"},
		Return: types.ExactFloat,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case Int:
				return Float(v), nil

			case *BigInt:
				return &BigFloat{
					F: big.NewFloat(0.0).SetInt(v.I),
				}, nil

			case Float, *BigFloat:
				return v, nil

			default:
				return Float(0), fmt.Errorf("invalid number: %v", v)
			}
		},
	},
	{
		Name:   "number->integer",
		Args:   []string{"x"},
		Return: types.ExactFloat,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case Int, *BigInt:
				return v, nil

			case Float:
				return Int(v), nil

			case *BigFloat:
				i, _ := v.F.Int(nil)
				return &BigInt{
					I: i,
				}, nil

			default:
				return Float(0), fmt.Errorf("invalid integer: %v", v)
			}
		},
	},

	{
		Name:   "remainder",
		Args:   []string{"n", "n"},
		Return: types.Number,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch n1 := args[0].(type) {
			case Int:
				switch n2 := args[1].(type) {
				case Int:
					return Int(n1 % n2), nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Rem(big.NewInt(int64(n1)), n2.I),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			case *BigInt:
				switch n2 := args[1].(type) {
				case Int:
					return &BigInt{
						I: new(big.Int).Rem(n1.I, big.NewInt(int64(n2))),
					}, nil

				case *BigInt:
					return &BigInt{
						I: new(big.Int).Rem(n1.I, n2.I),
					}, nil

				default:
					return Int(0), fmt.Errorf("invalid number: %v", args[1])
				}

			default:
				return Int(0), fmt.Errorf("expt: invalid number: %v", args[0])
			}
		},
	},
}

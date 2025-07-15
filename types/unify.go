//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"fmt"
)

// Unify resolves the closest supertype for the argument types.
func Unify(a *Type, b *Type) *Type {
	if a == nil {
		return b
	}
	if b == nil {
		return a
	}
	if a.Enum == EnumUnspecified || b.Enum == EnumUnspecified {
		return Unspecified
	}
	if a.Enum == EnumTypeVar || b.Enum == EnumTypeVar {
		return Unspecified
	}
	if a.Enum == EnumNone {
		return b
	}
	if b.Enum == EnumNone {
		return a
	}
	if a.IsKindOf(b) {
		return b
	}
	if b.IsKindOf(a) {
		return a
	}
	e := a.Enum.Unify(b.Enum)
	switch e {
	case EnumUnspecified:
		return Unspecified

	case EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol,
		EnumBytevector, EnumNumber, EnumExactInteger, EnumInexactInteger,
		EnumExactFloat, EnumInexactFloat, EnumPort:
		return &Type{
			Enum: e,
		}

	case EnumLambda:
		if len(a.Args) != len(b.Args) ||
			(a.Rest == nil && b.Rest != nil) ||
			(a.Rest != nil && b.Rest == nil) {
			return Any
		}
		t := &Type{
			Enum:   e,
			Rest:   Unify(a.Rest, b.Rest),
			Return: Unify(a.Return, b.Return),
		}
		for idx, arg := range a.Args {
			t.Args = append(t.Args, Unify(arg, b.Args[idx]))
		}
		return t

	case EnumPair:
		return &Type{
			Enum: e,
			Car:  Unify(a.Car, b.Car),
			Cdr:  Unify(a.Cdr, b.Cdr),
		}

	case EnumVector:
		return &Type{
			Enum:    e,
			Element: Unify(a.Element, b.Element),
		}

	case EnumNil:
		return Nil

	default:
		panic(fmt.Sprintf("unknown Enum: %v[%d]", e, e))
	}
}

// Coerce does type coercion for the numeric argument types. For other
// than numeric types, the function does Unify.
func Coerce(a *Type, b *Type) *Type {
	if a == nil {
		return b
	}
	if b == nil {
		return a
	}
	if a.Enum == EnumUnspecified || b.Enum == EnumUnspecified {
		return Unspecified
	}
	if !a.IsKindOf(Number) || !b.IsKindOf(Number) {
		return Unify(a, b)
	}
	if a.Enum == b.Enum {
		return a
	}
	switch a.Enum {
	case EnumInexactInteger:
		return b

	case EnumInexactFloat:
		switch b.Enum {
		case EnumInexactInteger:
			return InexactFloat
		default:
			return ExactFloat
		}

	case EnumExactInteger:
		switch b.Enum {
		case EnumInexactInteger:
			return ExactInteger
		default:
			return ExactFloat
		}

	default:
		return ExactFloat
	}
}

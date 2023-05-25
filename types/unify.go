//
// Copyright (c) 2023 Markku Rossi
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
	if a.IsKindOf(b) {
		return b
	}
	if b.IsKindOf(a) {
		return a
	}
	e := a.Enum.Unify(b.Enum)
	switch e {
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

	default:
		panic(fmt.Sprintf("unknown Enum: %d", e))
	}
}

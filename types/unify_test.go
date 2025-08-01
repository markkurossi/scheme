//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"testing"
)

func TestUnify(t *testing.T) {
	tests := []struct {
		a *Type
		b *Type
		u *Type
	}{
		{nil, nil, nil},
		{nil, Unspecified, Unspecified},
		{Unspecified, nil, Unspecified},
		{Any, nil, Any},
		{nil, Any, Any},

		{Any, Unspecified, Unspecified},
		{Unspecified, Any, Unspecified},

		{Any, Number, Any},
		{Number, Any, Any},
		{ExactInteger, Any, Any},
		{Any, ExactInteger, Any},
		{ExactInteger, Number, Number},
		{Number, ExactInteger, Number},

		{
			a: &Type{
				Enum:   EnumLambda,
				Args:   []*Type{Number, Number},
				Rest:   Number,
				Return: Number,
			},
			b: &Type{
				Enum:   EnumLambda,
				Args:   []*Type{InexactInteger, InexactFloat},
				Rest:   ExactInteger,
				Return: String,
			},
			u: &Type{
				Enum:   EnumLambda,
				Args:   []*Type{Number, Number},
				Rest:   Number,
				Return: Any,
			},
		},

		{
			a: &Type{
				Enum: EnumPair,
				Car:  Number,
				Cdr:  Number,
			},
			b: &Type{
				Enum: EnumPair,
				Car:  InexactInteger,
				Cdr:  InexactInteger,
			},
			u: &Type{
				Enum: EnumPair,
				Car:  Number,
				Cdr:  Number,
			},
		},
		{
			a: &Type{
				Enum:    EnumVector,
				Element: Number,
			},
			b: &Type{
				Enum:    EnumVector,
				Element: InexactFloat,
			},
			u: &Type{
				Enum:    EnumVector,
				Element: Number,
			},
		},
		{
			a: None,
			b: InexactInteger,
			u: InexactInteger,
		},
		{
			a: InexactInteger,
			b: None,
			u: InexactInteger,
		},
	}
	for idx, test := range tests {
		u := Unify(test.a, test.b)
		if (test.u == nil && u != nil) ||
			(test.u != nil && u == nil) ||
			(test.u != nil && !u.IsA(test.u)) {
			t.Errorf("TestUnify-%v: Unify(%v, %v)=%v, expected %v",
				idx, test.a, test.b, u, test.u)
		}
	}
}

func TestCoerce(t *testing.T) {
	tests := []struct {
		a *Type
		b *Type
		c *Type
	}{
		{Unspecified, InexactInteger, Unspecified},
	}

	for idx, test := range tests {
		c := Coerce(test.a, test.b)
		if !c.IsA(test.c) {
			t.Errorf("TestCoerce-%v: Coerce(%v, %v)=%v, expected %v",
				idx, test.a, test.b, c, test.c)
		}
	}
}

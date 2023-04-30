//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"testing"
)

func TestSuper(t *testing.T) {
	for _, e := range []Enum{
		EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol, EnumVector,
		EnumBytevector, EnumNumber, EnumLambda, EnumPair} {
		if e.Super() != EnumAny {
			t.Errorf("%v.Super() != %v", e, EnumAny)
		}
	}
	for _, e := range []Enum{EnumInteger, EnumFloat} {
		if e.Super() != EnumNumber {
			t.Errorf("%v.Super() != %v", e, EnumNumber)
		}
	}
	for _, e := range []Enum{EnumExactInteger, EnumInexactInteger} {
		if e.Super() != EnumInteger {
			t.Errorf("%v.Super() != %v", e, EnumInteger)
		}
	}
	for _, e := range []Enum{EnumExactFloat, EnumInexactFloat} {
		if e.Super() != EnumFloat {
			t.Errorf("%v.Super() != %v", e, EnumFloat)
		}
	}
}

func TestIsA(t *testing.T) {
	for _, typ := range []*Type{
		Any, Boolean, String, Character, Symbol, Vector, Bytevector,
		Number, Integer, ExactInteger, InexactInteger, Float,
		ExactFloat, InexactFloat} {
		if !typ.IsA(typ) {
			t.Errorf("!%v.IsA(%v)", typ, typ)
		}
	}

	lambda := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Integer, Integer},
		Rest:   Integer,
		Return: Integer,
	}
	if !lambda.IsA(lambda) {
		t.Errorf("!%v.IsA(%v)", lambda, lambda)
	}

	pair := &Type{
		Enum: EnumPair,
		Car:  Integer,
		Cdr:  Integer,
	}
	if !pair.IsA(pair) {
		t.Errorf("!%v.IsA(%v)", pair, pair)
	}
}

func TestIsKindOf(t *testing.T) {
	for _, typ := range []*Type{
		Any, Boolean, String, Character, Symbol, Vector, Bytevector,
		Number, Integer, ExactInteger, InexactInteger, Float,
		ExactFloat, InexactFloat} {
		if !typ.IsKindOf(typ) {
			t.Errorf("!%v.IsKindOf(%v)", typ, typ)
		}
		if !typ.IsKindOf(Any) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Any)
		}
	}
	for _, typ := range []*Type{
		Integer, ExactInteger, InexactInteger, Float,
		ExactFloat, InexactFloat} {
		if !typ.IsKindOf(Number) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Number)
		}
	}
	for _, typ := range []*Type{ExactInteger, InexactInteger} {
		if !typ.IsKindOf(Integer) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Integer)
		}
	}
	for _, typ := range []*Type{ExactFloat, InexactFloat} {
		if !typ.IsKindOf(Float) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Float)
		}
	}

	lambda := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Integer, Integer},
		Rest:   Integer,
		Return: Integer,
	}
	if !lambda.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, Any)
	}
	lambda1 := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Any, Any},
		Rest:   Integer,
		Return: Integer,
	}
	if !lambda.IsKindOf(lambda1) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, lambda1)
	}
	lambda2 := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Any, Any},
		Rest:   Any,
		Return: Integer,
	}
	if !lambda.IsKindOf(lambda2) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, lambda2)
	}
	lambda3 := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Any, Any},
		Rest:   Any,
		Return: Any,
	}
	if !lambda.IsKindOf(lambda3) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, lambda3)
	}

	pair := &Type{
		Enum: EnumPair,
		Car:  Integer,
		Cdr:  Integer,
	}
	if !pair.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", pair, Any)
	}
	pair1 := &Type{
		Enum: EnumPair,
		Car:  Any,
		Cdr:  Integer,
	}
	if !pair.IsKindOf(pair1) {
		t.Errorf("!%v.IsKindOf(%v)", pair, pair1)
	}
	pair2 := &Type{
		Enum: EnumPair,
		Car:  Any,
		Cdr:  Any,
	}
	if !pair.IsKindOf(pair2) {
		t.Errorf("!%v.IsKindOf(%v)", pair, pair2)
	}
}

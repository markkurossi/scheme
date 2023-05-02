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
		EnumBytevector, EnumNumber, EnumPort, EnumLambda, EnumPair} {
		if e.Super() != EnumAny {
			t.Errorf("%v.Super() != %v", e, EnumAny)
		}
	}
	for _, e := range []Enum{EnumExactInteger, EnumExactFloat} {
		if e.Super() != EnumNumber {
			t.Errorf("%v.Super() != %v", e, EnumNumber)
		}
	}
	for _, e := range []Enum{EnumInexactInteger} {
		if e.Super() != EnumExactInteger {
			t.Errorf("%v.Super() != %v", e, EnumExactInteger)
		}
	}
	for _, e := range []Enum{EnumInexactFloat} {
		if e.Super() != EnumExactFloat {
			t.Errorf("%v.Super() != %v", e, EnumExactFloat)
		}
	}
}

func TestEnumUnify(t *testing.T) {
	directs := []Enum{
		EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol,
		EnumBytevector, EnumNumber, EnumPort, EnumLambda, EnumPair,
		EnumList, EnumVector,
	}
	for _, a := range directs {
		for _, b := range directs {
			if a == b {
				testEnumUnify(t, a, b, a)
			} else {
				testEnumUnify(t, a, b, EnumAny)
			}
		}
	}
	testEnumUnify(t, EnumNumber, EnumExactInteger, EnumNumber)
	testEnumUnify(t, EnumNumber, EnumInexactInteger, EnumNumber)
	testEnumUnify(t, EnumNumber, EnumExactFloat, EnumNumber)
	testEnumUnify(t, EnumNumber, EnumInexactFloat, EnumNumber)

	testEnumUnify(t, EnumExactInteger, EnumInexactInteger, EnumExactInteger)
	testEnumUnify(t, EnumExactFloat, EnumInexactFloat, EnumExactFloat)
}

func testEnumUnify(t *testing.T, a, b, e Enum) {
	u := a.Unify(b)
	if u != e {
		t.Errorf("%v.Unify(%v) = %v, expected %v", a, b, u, e)
	}
}

func TestIsA(t *testing.T) {
	for _, typ := range []*Type{
		Any, Boolean, String, Character, Symbol, Bytevector, Number,
		ExactInteger, InexactInteger, ExactFloat, InexactFloat, Port} {
		if !typ.IsA(typ) {
			t.Errorf("!%v.IsA(%v)", typ, typ)
		}
	}

	lambda := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{ExactInteger, ExactInteger},
		Rest:   ExactInteger,
		Return: ExactInteger,
	}
	if !lambda.IsA(lambda) {
		t.Errorf("!%v.IsA(%v)", lambda, lambda)
	}

	pair := &Type{
		Enum: EnumPair,
		Car:  ExactInteger,
		Cdr:  ExactInteger,
	}
	if !pair.IsA(pair) {
		t.Errorf("!%v.IsA(%v)", pair, pair)
	}

	list := &Type{
		Enum:    EnumList,
		Element: ExactInteger,
	}
	if !list.IsA(list) {
		t.Errorf("!%v.IsA(%v)", list, list)
	}

	vector := &Type{
		Enum:    EnumVector,
		Element: ExactInteger,
	}
	if !vector.IsA(vector) {
		t.Errorf("!%v.IsA(%v)", vector, vector)
	}
}

func TestIsKindOf(t *testing.T) {
	for _, typ := range []*Type{
		Any, Boolean, String, Character, Symbol, Bytevector, Number,
		ExactInteger, InexactInteger, ExactFloat, InexactFloat, Port} {
		if !typ.IsKindOf(typ) {
			t.Errorf("!%v.IsKindOf(%v)", typ, typ)
		}
		if !typ.IsKindOf(Any) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Any)
		}
	}
	for _, typ := range []*Type{
		ExactInteger, InexactInteger, ExactFloat, InexactFloat} {
		if !typ.IsKindOf(Number) {
			t.Errorf("!%v.IsKindOf(%v)", typ, Number)
		}
	}
	for _, typ := range []*Type{InexactFloat} {
		if !typ.IsKindOf(ExactFloat) {
			t.Errorf("!%v.IsKindOf(%v)", typ, ExactFloat)
		}
	}

	lambda := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{ExactInteger, ExactInteger},
		Rest:   ExactInteger,
		Return: ExactInteger,
	}
	if !lambda.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, Any)
	}
	lambda1 := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Any, Any},
		Rest:   ExactInteger,
		Return: ExactInteger,
	}
	if !lambda.IsKindOf(lambda1) {
		t.Errorf("!%v.IsKindOf(%v)", lambda, lambda1)
	}
	lambda2 := &Type{
		Enum:   EnumLambda,
		Args:   []*Type{Any, Any},
		Rest:   Any,
		Return: ExactInteger,
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
		Car:  ExactInteger,
		Cdr:  ExactInteger,
	}
	if !pair.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", pair, Any)
	}
	pair1 := &Type{
		Enum: EnumPair,
		Car:  Any,
		Cdr:  ExactInteger,
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

	list := &Type{
		Enum:    EnumList,
		Element: ExactInteger,
	}
	if !list.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", list, Any)
	}
	list1 := &Type{
		Enum:    EnumList,
		Element: Any,
	}
	if !list.IsKindOf(list1) {
		t.Errorf("!%v.IsKindOf(%v)", list, list1)
	}

	vector := &Type{
		Enum:    EnumVector,
		Element: ExactInteger,
	}
	if !vector.IsKindOf(Any) {
		t.Errorf("!%v.IsKindOf(%v)", vector, Any)
	}
	vector1 := &Type{
		Enum:    EnumVector,
		Element: Any,
	}
	if !vector.IsKindOf(vector1) {
		t.Errorf("!%v.IsKindOf(%v)", vector, vector1)
	}
}

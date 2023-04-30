//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"fmt"
)

type Enum int

const (
	EnumAny Enum = iota
	EnumBoolean
	EnumString
	EnumCharacter
	EnumSymbol
	EnumVector
	EnumBytevector
	EnumNumber
	EnumInteger
	EnumExactInteger
	EnumInexactInteger
	EnumFloat
	EnumExactFloat
	EnumInexactFloat
	EnumLambda
	EnumPair
)

var enumNames = map[Enum]string{
	EnumAny:            "Any",
	EnumBoolean:        "Boolean",
	EnumString:         "String",
	EnumCharacter:      "Character",
	EnumSymbol:         "Symbol",
	EnumVector:         "Vector",
	EnumBytevector:     "Bytevector",
	EnumNumber:         "Number",
	EnumInteger:        "Integer",
	EnumExactInteger:   "ExactInteger",
	EnumInexactInteger: "InexactInteger",
	EnumFloat:          "Float",
	EnumExactFloat:     "ExactFloat",
	EnumInexactFloat:   "InexactFloat",
	EnumLambda:         "Lambda",
	EnumPair:           "Pair",
}

func (e Enum) String() string {
	name, ok := enumNames[e]
	if ok {
		return name
	}
	return fmt.Sprintf("{Enum %d}", e)
}

func (e Enum) Super() Enum {
	switch e {
	case EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol,
		EnumVector, EnumBytevector, EnumNumber, EnumLambda, EnumPair:
		return EnumAny

	case EnumInteger, EnumFloat:
		return EnumNumber

	case EnumExactInteger, EnumInexactInteger:
		return EnumInteger

	case EnumExactFloat, EnumInexactFloat:
		return EnumFloat

	default:
		panic(fmt.Sprintf("unknown Enum %d", e))
	}
}

type Type struct {
	Enum   Enum
	Args   []*Type
	Rest   *Type
	Return *Type
	Car    *Type
	Cdr    *Type
}

func (t *Type) String() string {
	result := t.Enum.String()

	switch t.Enum {
	case EnumLambda:
		result += "("
		for idx, arg := range t.Args {
			if idx > 0 {
				result += ","
			}
			result += arg.String()
		}
		if t.Rest != nil {
			result += " . "
			result += t.Rest.String()
		}
		return result + ")" + t.Return.String()

	case EnumPair:
		return result + "(" + t.Car.String() + "," + t.Cdr.String() + ")"

	default:
		return result
	}
}

var (
	Any = &Type{
		Enum: EnumAny,
	}
	Boolean = &Type{
		Enum: EnumBoolean,
	}
	String = &Type{
		Enum: EnumString,
	}
	Character = &Type{
		Enum: EnumCharacter,
	}
	Symbol = &Type{
		Enum: EnumSymbol,
	}
	Vector = &Type{
		Enum: EnumVector,
	}
	Bytevector = &Type{
		Enum: EnumBytevector,
	}
	Number = &Type{
		Enum: EnumNumber,
	}
	Integer = &Type{
		Enum: EnumInteger,
	}
	ExactInteger = &Type{
		Enum: EnumExactInteger,
	}
	InexactInteger = &Type{
		Enum: EnumInexactInteger,
	}
	Float = &Type{
		Enum: EnumFloat,
	}
	ExactFloat = &Type{
		Enum: EnumExactFloat,
	}
	InexactFloat = &Type{
		Enum: EnumInexactFloat,
	}
)

func (t *Type) IsA(o *Type) bool {
	if t.Enum != o.Enum {
		return false
	}
	switch t.Enum {
	case EnumLambda:
		if len(t.Args) != len(o.Args) {
			return false
		}
		for idx, arg := range t.Args {
			if !arg.IsA(o.Args[idx]) {
				return false
			}
		}
		if t.Rest != nil && o.Rest != nil {
			if !t.Rest.IsA(o.Rest) {
				return false
			}
		} else if t.Rest == nil && o.Rest == nil {
		} else {
			return false
		}
		return t.Return.IsA(o.Return)

	case EnumPair:
		return t.Car.IsA(o.Car) && t.Cdr.IsA(o.Cdr)

	default:
		return true
	}
}

func (t *Type) IsKindOf(o *Type) bool {
	e := t.Enum
	for {
		if e == o.Enum {
			break
		}
		n := e.Super()
		if n == e {
			return false
		}
		e = n
	}
	if t.Enum != o.Enum {
		return true
	}

	switch t.Enum {
	case EnumLambda:
		if len(t.Args) != len(o.Args) {
			return false
		}
		for idx, arg := range t.Args {
			if !arg.IsKindOf(o.Args[idx]) {
				return false
			}
		}
		if t.Rest != nil && o.Rest != nil {
			if !t.Rest.IsKindOf(o.Rest) {
				return false
			}
		} else if t.Rest == nil && o.Rest == nil {
		} else {
			return false
		}
		return t.Return.IsKindOf(o.Return)

	case EnumPair:
		return t.Car.IsKindOf(o.Car) && t.Cdr.IsKindOf(o.Cdr)

	default:
		return true
	}
}

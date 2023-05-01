//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"fmt"
	"strings"
)

// Enum defines type values.
type Enum int

// Known type values.
const (
	EnumAny Enum = iota
	EnumBoolean
	EnumString
	EnumCharacter
	EnumSymbol
	EnumBytevector
	EnumNumber
	EnumInteger
	EnumExactInteger
	EnumInexactInteger
	EnumFloat
	EnumExactFloat
	EnumInexactFloat
	EnumPort
	EnumLambda
	EnumPair
	EnumList
	EnumVector
)

var enumNames = map[Enum]string{
	EnumAny:            "Any",
	EnumBoolean:        "Boolean",
	EnumString:         "String",
	EnumCharacter:      "Character",
	EnumSymbol:         "Symbol",
	EnumBytevector:     "Bytevector",
	EnumNumber:         "Number",
	EnumInteger:        "Integer",
	EnumExactInteger:   "ExactInteger",
	EnumInexactInteger: "InexactInteger",
	EnumFloat:          "Float",
	EnumExactFloat:     "ExactFloat",
	EnumInexactFloat:   "InexactFloat",
	EnumPort:           "Port",
	EnumLambda:         "Lambda",
	EnumPair:           "Pair",
	EnumList:           "List",
	EnumVector:         "Vector",
}

func (e Enum) String() string {
	name, ok := enumNames[e]
	if ok {
		return name
	}
	return fmt.Sprintf("{Enum %d}", e)
}

// Super returns the type enum's supertype.
func (e Enum) Super() Enum {
	switch e {
	case EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol,
		EnumBytevector, EnumNumber, EnumPort, EnumLambda, EnumPair,
		EnumList, EnumVector:
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

// Parse parses the type of the function argument based on naming
// conventions.
func Parse(arg string) (*Type, string, error) {
	name := arg
	idx := strings.IndexRune(arg, ':')
	if idx >= 0 {
		name = arg[idx+1:]
	}
	if arg[0] == '[' {
		if idx >= 0 {
			name = "[" + name
		}
		arg = arg[1:]
	}

	if strings.HasPrefix(arg, "bool") {
		return Boolean, name, nil
	} else if strings.HasPrefix(arg, "bytevector") {
		return Bytevector, name, nil
	} else if strings.HasPrefix(arg, "char") {
		return Character, name, nil
	} else if strings.HasPrefix(arg, "k") {
		return Integer, name, nil
	} else if strings.HasPrefix(arg, "list") {
		return &Type{
			Enum:    EnumList,
			Element: Any,
		}, name, nil
	} else if strings.HasPrefix(arg, "obj") || strings.HasPrefix(arg, "who") ||
		strings.HasPrefix(arg, "irritant") {
		return Any, name, nil
	} else if strings.HasPrefix(arg, "pair") {
		return &Type{
			Enum: EnumPair,
			Car:  Any,
			Cdr:  Any,
		}, name, nil
	} else if strings.HasPrefix(arg, "port") {
		return Port, name, nil
	} else if strings.HasPrefix(arg, "string") ||
		strings.HasPrefix(arg, "message") {
		return String, name, nil
	} else if strings.HasPrefix(arg, "sym") {
		return Symbol, name, nil
	} else if strings.HasPrefix(arg, "vector") {
		return &Type{
			Enum:    EnumVector,
			Element: Any,
		}, name, nil
	} else if strings.HasPrefix(arg, "x") {
		return Number, name, nil
	} else if strings.HasPrefix(arg, "z") {
		return Number, name, nil
	} else if strings.HasPrefix(arg, "start") || strings.HasPrefix(arg, "end") {
		return Integer, name, nil
	} else {
		return nil, name, fmt.Errorf("unsupported argument: %v", arg)
	}
}

// Type defines Scheme types.
type Type struct {
	Enum    Enum
	Args    []*Type
	Rest    *Type
	Return  *Type
	Car     *Type
	Cdr     *Type
	Element *Type
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

	case EnumList:
		return result + "(" + t.Element.String() + ")"

	default:
		return result
	}
}

// Basic types.
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
	Port = &Type{
		Enum: EnumPort,
	}
)

// IsA tests if type is the same as the argument type.
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

	case EnumList, EnumVector:
		return t.Element.IsA(o.Element)

	default:
		return true
	}
}

// IsKindOf tests if type is kind of the argument type.
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

	case EnumList, EnumVector:
		return t.Element.IsKindOf(o.Element)

	default:
		return true
	}
}

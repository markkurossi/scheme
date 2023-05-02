//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"fmt"
	"regexp"
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
	EnumExactInteger
	EnumInexactInteger
	EnumExactFloat
	EnumInexactFloat
	EnumPort
	EnumLambda
	EnumPair
	EnumList
	EnumVector
)

var enumNames = map[Enum]string{
	EnumAny:            "any",
	EnumBoolean:        "bool",
	EnumString:         "string",
	EnumCharacter:      "char",
	EnumSymbol:         "symbol",
	EnumBytevector:     "bytevector",
	EnumNumber:         "number",
	EnumExactInteger:   "#eint",
	EnumInexactInteger: "int",
	EnumExactFloat:     "#efloat",
	EnumInexactFloat:   "float",
	EnumPort:           "port",
	EnumLambda:         "lambda",
	EnumPair:           "pair",
	EnumList:           "list",
	EnumVector:         "vector",
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

	case EnumExactInteger, EnumExactFloat:
		return EnumNumber

	case EnumInexactInteger:
		return EnumExactInteger

	case EnumInexactFloat:
		return EnumExactFloat

	default:
		panic(fmt.Sprintf("unknown Enum %d", e))
	}
}

var (
	reArgType = regexp.MustCompilePOSIX(`^(\[?)([^<]+)(<([a-z0-9]+)>)?(\]?)$`)
)

// Parse parses the type of the function argument based on naming
// conventions.
func Parse(arg string) (*Type, string, error) {
	m := reArgType.FindStringSubmatch(arg)
	if m == nil {
		panic(fmt.Sprintf("*** Parse: no match: %v", arg))
	}
	var opt bool
	var name string
	var typeName string

	if len(m[1]) > 0 {
		opt = true
	}
	name = m[2]
	if len(m[4]) > 0 {
		typeName = m[4]
	} else {
		typeName = m[2]
	}

	if opt {
		name = "[" + name + "]"
	}

	if strings.HasPrefix(typeName, "bool") {
		return Boolean, name, nil
	} else if strings.HasPrefix(typeName, "bytevector") {
		return Bytevector, name, nil
	} else if strings.HasPrefix(typeName, "char") {
		return Character, name, nil
	} else if strings.HasPrefix(typeName, "k") || typeName == "int" {
		return InexactInteger, name, nil
	} else if strings.HasPrefix(typeName, "list") {
		return &Type{
			Enum:    EnumList,
			Element: Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "obj") ||
		strings.HasPrefix(typeName, "who") ||
		strings.HasPrefix(typeName, "irritant") || typeName == "any" {
		return Any, name, nil
	} else if strings.HasPrefix(typeName, "pair") {
		return &Type{
			Enum: EnumPair,
			Car:  Any,
			Cdr:  Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "port") {
		return Port, name, nil
	} else if strings.HasPrefix(typeName, "string") ||
		strings.HasPrefix(typeName, "message") {
		return String, name, nil
	} else if strings.HasPrefix(typeName, "sym") {
		return Symbol, name, nil
	} else if strings.HasPrefix(typeName, "vector") {
		return &Type{
			Enum:    EnumVector,
			Element: Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "x") {
		return Number, name, nil
	} else if strings.HasPrefix(typeName, "z") {
		return Number, name, nil
	} else if strings.HasPrefix(typeName, "start") ||
		strings.HasPrefix(typeName, "end") {
		return InexactInteger, name, nil
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
	ExactInteger = &Type{
		Enum: EnumExactInteger,
	}
	InexactInteger = &Type{
		Enum: EnumInexactInteger,
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

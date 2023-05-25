//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package types

import (
	"fmt"
	"math"
	"regexp"
	"strings"
)

// Enum defines type values.
type Enum int

// Known type values.
const (
	EnumUnspecified Enum = iota
	EnumAny
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
	EnumVector
)

var enumNames = map[Enum]string{
	EnumUnspecified:    "?",
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
	case EnumUnspecified:
		return EnumUnspecified

	case EnumAny, EnumBoolean, EnumString, EnumCharacter, EnumSymbol,
		EnumBytevector, EnumNumber, EnumPort, EnumLambda, EnumPair, EnumVector:
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

// Unify resolves the closest supertype of this and the argument
// enum.
func (e Enum) Unify(o Enum) Enum {
	if e == EnumUnspecified || o == EnumUnspecified {
		return EnumUnspecified
	}

	for eIter := e; ; eIter = eIter.Super() {
		for oIter := o; ; oIter = oIter.Super() {
			if eIter == oIter {
				return eIter
			}
			if oIter == EnumAny {
				break
			}
		}
	}
}

var (
	reArgType = regexp.MustCompilePOSIX(
		`^(\[?)([^<\]]+)(<([a-z0-9]+)>)?(\.\.\.)?(\]?)$`)
)

// Parse parses the type of the function argument based on naming
// conventions.
func Parse(arg string) (*Type, string, error) {
	m := reArgType.FindStringSubmatch(arg)
	if m == nil {
		panic(fmt.Sprintf("types.Parse: no match: %v", arg))
	}
	var name string
	var kind Kind
	var typeName string

	if len(m[1]) > 0 {
		kind = Optional
	}
	if len(m[5]) > 0 {
		kind = Rest
	}
	name = m[2]
	if strings.HasSuffix(name, "...") {
		kind = Rest
		name = name[:len(name)-3]
	}

	if len(m[4]) > 0 {
		typeName = m[4]
	} else {
		typeName = name
	}

	if strings.HasPrefix(typeName, "bool") {
		return &Type{
			Enum: EnumBoolean,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "bytevector") {
		return &Type{
			Enum: EnumBytevector,
			Kind: kind,
		}, name, nil
	} else if typeName == "chars" {
		return &Type{
			Enum: EnumPair,
			Kind: kind,
			Car:  Character,
			Cdr:  Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "char") {
		return &Type{
			Enum: EnumCharacter,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "k") || typeName == "int" {
		return &Type{
			Enum: EnumInexactInteger,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "list") {
		return &Type{
			Enum: EnumPair,
			Kind: kind,
			Car:  Any,
			Cdr:  Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "obj") ||
		strings.HasPrefix(typeName, "who") ||
		strings.HasPrefix(typeName, "irritant") || typeName == "any" {
		return &Type{
			Enum: EnumAny,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "pair") {
		return &Type{
			Enum: EnumPair,
			Kind: kind,
			Car:  Any,
			Cdr:  Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "port") {
		return &Type{
			Enum: EnumPort,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "string") ||
		strings.HasPrefix(typeName, "message") {
		return &Type{
			Enum: EnumString,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "sym") {
		return &Type{
			Enum: EnumSymbol,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "vector") {
		return &Type{
			Enum:    EnumVector,
			Kind:    kind,
			Element: Any,
		}, name, nil
	} else if strings.HasPrefix(typeName, "x") ||
		strings.HasPrefix(typeName, "z") {
		return &Type{
			Enum: EnumNumber,
			Kind: kind,
		}, name, nil
	} else if strings.HasPrefix(typeName, "start") ||
		strings.HasPrefix(typeName, "end") {
		return &Type{
			Enum: EnumInexactInteger,
			Kind: kind,
		}, name, nil
	} else {
		return Unspecified, name, fmt.Errorf("unsupported argument: %v", arg)
	}
}

// Type defines Scheme types.
type Type struct {
	Enum    Enum
	Kind    Kind
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
		result = result + ")" + t.Return.String()

	case EnumPair:
		result = result + "(" + t.Car.String() + "," + t.Cdr.String() + ")"

	case EnumVector:
		result = result + "(" + t.Element.String() + ")"

	default:
	}

	switch t.Kind {
	case Optional:
		return "[" + result + "]"
	case Rest:
		return result + "..."
	default:
		return result
	}
}

// Basic types.
var (
	Unspecified = &Type{
		Enum: EnumUnspecified,
	}
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
	Pair = &Type{
		Enum: EnumPair,
		Car:  Any,
		Cdr:  Any,
	}
	Nil = Unspecified
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

	case EnumVector:
		return t.Element.IsA(o.Element)

	default:
		return true
	}
}

// IsKindOf tests if type is kind of the argument type.
func (t *Type) IsKindOf(o *Type) bool {
	if t.Enum == EnumUnspecified || o.Enum == EnumUnspecified {
		return true
	}

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

	case EnumVector:
		return t.Element.IsKindOf(o.Element)

	default:
		return true
	}
}

// MinArgs returns the minimum number for arguments for a lambda
// type. For all other types the function returns 0.
func (t *Type) MinArgs() int {
	if t.Enum != EnumLambda {
		return 0
	}
	var count int
	for _, arg := range t.Args {
		if arg.Kind == Fixed {
			count++
		}
	}
	return count
}

// MaxArgs returns the maximum number for arguments for a lambda
// type. For all other types the function returns 0.
func (t *Type) MaxArgs() int {
	if t.Enum != EnumLambda {
		return 0
	}
	if t.Rest != nil {
		return math.MaxInt
	}
	var count int
	for _, arg := range t.Args {
		if arg.Kind == Rest {
			return math.MaxInt
		}
		count++
	}
	return count
}

// Kind specifies argument type (fixed, optional, rest).
type Kind int

// Argument types.
const (
	Fixed Kind = iota
	Optional
	Rest
)

var kinds = map[Kind]string{
	Fixed:    "fixed",
	Optional: "optional",
	Rest:     "rest",
}

func (k Kind) String() string {
	name, ok := kinds[k]
	if ok {
		return name
	}
	return fmt.Sprintf("{Kind %d}", k)
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"strings"
)

var (
	_ Value = &Cons{}
	_ Value = &Vector{}
	_ Value = &Identifier{}
	_ Value = &Number{}
	_ Value = &Boolean{}
	_ Value = &String{}
	_ Value = &Character{}
)

// ValueType specifies value types.
type ValueType int

// Value types.
const (
	VCons ValueType = iota
	VVector
	VIdentifier
	VNumber
	VBoolean
	VString
	VCharacter
)

// Value implements a Scheme value.
type Value interface {
	Type() ValueType
}

// Cons implements cons cell values.
type Cons struct {
	Car Value
	Cdr Value
}

// Type returns the cons cell value type.
func (v *Cons) Type() ValueType {
	return VCons
}

func (v *Cons) String() string {
	var str strings.Builder
	str.WriteRune('(')

	i := v
	first := true
loop:
	for {
		if first {
			first = false
		} else {
			str.WriteRune(' ')
		}
		str.WriteString(fmt.Sprintf("%v", i.Car))
		switch cdr := i.Cdr.(type) {
		case *Cons:
			i = cdr

		case nil:
			break loop

		default:
			str.WriteString(" . ")
			str.WriteString(fmt.Sprintf("%v", i.Cdr))
			break loop
		}
	}
	str.WriteRune(')')

	return str.String()
}

// Vector implements vector values.
type Vector struct {
	Elements []Value
}

// Type returns the vector value type.
func (v *Vector) Type() ValueType {
	return VVector
}

// Identifier implements identifier values.
type Identifier struct {
	Name   string
	Global Value
}

// Type returns the identifier value type.
func (v *Identifier) Type() ValueType {
	return VIdentifier
}

func (v *Identifier) String() string {
	return v.Name
}

// Boolean implements boolean values.
type Boolean struct {
	Bool bool
}

// Type returns the boolean value type.
func (v *Boolean) Type() ValueType {
	return VBoolean
}

func (v *Boolean) String() string {
	return BooleanToScheme(v.Bool)
}

// BooleanToScheme returns the bool as Scheme boolean literal.
func BooleanToScheme(v bool) string {
	var ch rune
	if v {
		ch = 't'
	} else {
		ch = 'f'
	}
	return fmt.Sprintf("#%c", ch)
}

// String implements string values.
type String struct {
	Str string
}

// Type returns the string value type.
func (v *String) Type() ValueType {
	return VString
}

func (v *String) String() string {
	return StringToScheme(v.Str)
}

// StringToScheme returns the string as Scheme string literal.
func StringToScheme(s string) string {
	var str strings.Builder
	str.WriteRune('"')
	for _, r := range s {
		switch r {
		case '\\', '"', '|', '(':
			str.WriteRune('\\')
			str.WriteRune(r)
		case '\a':
			str.WriteRune('\\')
			str.WriteRune('a')
		case '\f':
			str.WriteRune('\\')
			str.WriteRune('f')
		case '\n':
			str.WriteRune('\\')
			str.WriteRune('n')
		case '\r':
			str.WriteRune('\\')
			str.WriteRune('r')
		case '\t':
			str.WriteRune('\\')
			str.WriteRune('t')
		case '\v':
			str.WriteRune('\\')
			str.WriteRune('v')
		case '\b':
			str.WriteRune('\\')
			str.WriteRune('b')
		case 0:
			str.WriteRune('\\')
			str.WriteRune('0')
		default:
			str.WriteRune(r)
		}
	}
	str.WriteRune('"')
	return str.String()
}

// Character implements character values.
type Character struct {
	Char rune
}

// Type returns the character value type.
func (v *Character) Type() ValueType {
	return VCharacter
}

func (v *Character) String() string {
	return CharacterToScheme(v.Char)
}

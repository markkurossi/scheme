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
	_ Value = &Vector{}
	_ Value = &Identifier{}
	_ Value = Keyword(0)
	_ Value = &Number{}
	_ Value = Boolean(true)
	_ Value = &String{}
	_ Value = Character('@')
	_ Value = &Lambda{}
	_ Value = &Frame{}
)

// Value implements a Scheme value.
type Value interface {
	// Type() ValueType
	Scheme() string
}

// Vector implements vector values.
type Vector struct {
	Elements []Value
}

// Scheme returns the value as a Scheme string.
func (v *Vector) Scheme() string {
	return v.String()
}

func (v *Vector) String() string {
	var str strings.Builder
	str.WriteString("#(")

	for idx, el := range v.Elements {
		if idx > 0 {
			str.WriteRune(' ')
		}
		str.WriteString(fmt.Sprintf("%v", el))
	}
	str.WriteRune(')')
	return str.String()
}

// Identifier implements identifier values.
type Identifier struct {
	Name   string
	Global Value
}

// Scheme returns the value as a Scheme string.
func (v *Identifier) Scheme() string {
	return v.String()
}

func (v *Identifier) String() string {
	return v.Name
}

// Boolean implements boolean values.
type Boolean bool

// Scheme returns the value as a Scheme string.
func (v Boolean) Scheme() string {
	return v.String()
}

func (v Boolean) String() string {
	return BooleanToScheme(bool(v))
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

// Lambda implements lambda values.
type Lambda struct {
	MinArgs int
	MaxArgs int
	Args    []*Identifier
	Capture int
	Locals  [][]Value
	Native  Native
	Code    Code
	Body    Pair
}

// Scheme returns the value as a Scheme string.
func (v *Lambda) Scheme() string {
	return v.String()
}

func (v *Lambda) String() string {
	var str strings.Builder

	str.WriteString("(lambda (")
	for idx, arg := range v.Args {
		if idx > 0 {
			str.WriteRune(' ')
		}
		str.WriteString(arg.Name)
	}
	str.WriteRune(')')

	if v.Native != nil {
		str.WriteString(" {native}")
	} else {
		Map(func(v Value) error {
			str.WriteRune(' ')
			str.WriteString(fmt.Sprintf("%v", v))
			return nil
		}, v.Body)
	}
	str.WriteRune(')')

	return str.String()
}

// Native implements native functions.
type Native func(vm *VM, args []Value) (Value, error)

// Builtin defines a built-in native function.
type Builtin struct {
	Name    string
	Args    []string
	MinArgs int
	MaxArgs int
	Native  Native
}

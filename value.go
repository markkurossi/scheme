//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
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
	Scheme() string
	Eq(o Value) bool
	Equal(o Value) bool
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

// Eq tests if the argument value is eq? to this value.
func (v *Identifier) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (v *Identifier) Equal(o Value) bool {
	ov, ok := o.(*Identifier)
	return ok && v.Name == ov.Name
}

func (v *Identifier) String() string {
	return v.Name
}

// Lambda implements lambda values.
type Lambda struct {
	Name    string
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

// Eq tests if the argument value is eq? to this value.
func (v *Lambda) Eq(o Value) bool {
	return v == v
}

// Equal tests if the argument value is equal to this value.
func (v *Lambda) Equal(o Value) bool {
	ov, ok := o.(*Lambda)
	if !ok {
		return false
	}
	if len(v.Args) != len(ov.Args) {
		return false
	}
	if v.Capture != ov.Capture {
		return false
	}
	if v.Native == nil && ov.Native != nil {
		return false
	}
	if v.Native != nil && ov.Native == nil {
		return false
	}
	return v.Body.Equal(ov.Body)
}

func (v *Lambda) String() string {
	var str strings.Builder

	if v.Native != nil {
		str.WriteRune('(')
		str.WriteString(v.Name)
		str.WriteString(" (")
	} else {
		str.WriteString("(lambda (")
	}
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
		Map(func(idx int, v Value) error {
			str.WriteRune(' ')
			str.WriteString(fmt.Sprintf("%v", v))
			return nil
		}, v.Body)
	}
	str.WriteRune(')')

	return str.String()
}

// Errorf creates an error with information about the lambda function.
func (v *Lambda) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if len(v.Name) != 0 {
		return fmt.Errorf("%s: %s", v.Name, msg)
	}
	return errors.New(msg)
}

// Native implements native functions.
type Native func(scm *Scheme, l *Lambda, args []Value) (Value, error)

// Builtin defines a built-in native function.
type Builtin struct {
	Name    string
	Args    []string
	MinArgs int
	MaxArgs int
	Native  Native
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

// Boolean implements boolean values.
type Boolean bool

// Scheme returns the value as a Scheme string.
func (v Boolean) Scheme() string {
	return v.String()
}

// Equal tests if the argument value is equal to this value.
func (v Boolean) Equal(o Value) bool {
	ov, ok := o.(Boolean)
	return ok && v == ov
}

func (v Boolean) String() string {
	return BooleanToScheme(bool(v))
}

// True tests if the argument value is true.
func True(v Value) bool {
	b, ok := v.(Boolean)
	if !ok {
		return true
	}
	return bool(b)
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

var booleanBuiltins = []Builtin{
	{
		Name: "not",
		Args: []string{"obj"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return Boolean(!True(args[0])), nil
		},
	},
	{
		Name: "boolean?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(Boolean)
			return Boolean(ok), nil
		},
	},
}

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

// String implements string values.
type String []byte

// Scheme returns the value as a Scheme string.
func (v String) Scheme() string {
	return StringToScheme(string(v))
}

// Equal tests if the argument value is equal to this value.
func (v String) Equal(o Value) bool {
	ov, ok := o.(String)
	if !ok || len(v) != len(ov) {
		return false
	}
	for i := 0; i < len(v); i++ {
		if v[i] != ov[i] {
			return false
		}
	}
	return true
}

func (v String) String() string {
	return string(v)
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

var stringBuiltins = []Builtin{
	{
		Name: "string-length",
		Args: []string{"string"},
		Native: func(vm *VM, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case String:
				return NewNumber(0, len(v)), nil

			default:
				return nil, fmt.Errorf("string-length: invalid argument")
			}
		},
	},
}

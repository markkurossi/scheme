//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
)

// Vector implements vector values.
type Vector struct {
	Elements []Value
}

// Scheme returns the value as a Scheme string.
func (v *Vector) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v *Vector) Eq(o Value) bool {
	return v == o
}

// Equal tests if the argument value is equal to this value.
func (v *Vector) Equal(o Value) bool {
	ov, ok := o.(*Vector)
	if !ok || len(v.Elements) != len(ov.Elements) {
		return false
	}
	for idx, v := range v.Elements {
		if !v.Equal(ov.Elements[idx]) {
			return false
		}
	}
	return true
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

var vectorBuiltins = []Builtin{
	{
		Name: "vector?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			_, ok := args[0].(*Vector)
			return Boolean(ok), nil
		},
	},
	// XXX make-vector
	// XXX vector
	{
		Name: "vector-length",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			v, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("not a vector: %v", args[0])
			}
			return NewNumber(0, len(v.Elements)), nil
		},
	},
}

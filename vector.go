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
	{
		Name: "make-vector",
		Args: []string{"k", "[fill]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			k, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid length: %v", args[0])
			}
			length := k.Int64()
			if length < 0 {
				return nil, l.Errorf("negative length: %v", k)
			}

			var fill Value
			if len(args) == 2 {
				fill = args[1]
			}
			elements := make([]Value, length, length)
			for i := 0; i < int(length); i++ {
				elements[i] = fill
			}

			return &Vector{
				Elements: elements,
			}, nil
		},
	},
	{
		Name: "vector",
		Args: []string{"[obj]..."},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var elements []Value
			for i := 0; i < len(args); i++ {
				elements = append(elements, args[i])
			}
			return &Vector{
				Elements: elements,
			}, nil
		},
	},
	{
		Name: "vector-length",
		Args: []string{"vector"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			v, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("not a vector: %v", args[0])
			}
			return NewNumber(0, len(v.Elements)), nil
		},
	},
	{
		Name: "vector-ref",
		Args: []string{"vector", "k"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			vector, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("invalid vector: %v", args[0])
			}
			kn, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid index: %v", args[1])
			}
			k := int(kn.Int64())
			if k < 0 || k >= len(vector.Elements) {
				return nil, l.Errorf("index %v out of range for vector %v",
					k, args[0])
			}
			return vector.Elements[k], nil
		},
	},
	{
		Name: "vector-set!",
		Args: []string{"vector", "k", "obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			vector, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("invalid vector: %v", args[0])
			}
			kn, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid index: %v", args[1])
			}
			k := int(kn.Int64())
			if k < 0 || k >= len(vector.Elements) {
				return nil, l.Errorf("index %v out of range for vector %v",
					k, args[0])
			}
			vector.Elements[k] = args[2]
			return args[0], nil
		},
	},
	{
		Name: "vector->list",
		Args: []string{"vector"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			vector, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("invalid vector: %v", args[0])
			}
			var pair Pair

			for i := len(vector.Elements) - 1; i >= 0; i-- {
				pair = NewPair(vector.Elements[i], pair)
			}
			return pair, nil
		},
	},
	{
		Name: "list->vector",
		Args: []string{"list"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var elements []Value
			err := Map(func(idx int, v Value) error {
				elements = append(elements, v)
				return nil
			}, args[0])
			if err != nil {
				return nil, err
			}
			return &Vector{
				Elements: elements,
			}, nil
		},
	},
	{
		Name: "vector-fill!",
		Args: []string{"vector", "fill"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			vector, ok := args[0].(*Vector)
			if !ok {
				return nil, l.Errorf("invalid vector: %v", args[0])
			}
			for i := 0; i < len(vector.Elements); i++ {
				vector.Elements[i] = args[1]
			}
			return args[0], nil
		},
	},
}

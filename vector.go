//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"

	"github.com/markkurossi/scheme/types"
)

// Vector implements vector values.
type Vector []Value

// Scheme returns the value as a Scheme string.
func (v Vector) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v Vector) Eq(o Value) bool {
	ov, ok := o.(Vector)
	if !ok {
		return false
	}
	if len(v) == 0 && len(ov) == 0 {
		return true
	}
	return false
}

// Equal tests if the argument value is equal to this value.
func (v Vector) Equal(o Value) bool {
	ov, ok := o.(Vector)
	if !ok || len(v) != len(ov) {
		return false
	}
	for idx, vv := range v {
		if !vv.Equal(ov[idx]) {
			return false
		}
	}
	return true
}

// Type implements the Value.Type().
func (v Vector) Type() *types.Type {
	t := &types.Type{
		Enum: types.EnumVector,
	}
	for _, el := range v {
		if el != nil {
			t.Element = types.Unify(t.Element, el.Type())
		}
	}
	if t.Element == nil {
		t.Element = types.Unspecified
	}
	return t
}

// Unbox implements Value.Unbox.
func (v Vector) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

func (v Vector) String() string {
	var str strings.Builder
	str.WriteString("#(")

	for idx, el := range v {
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
		Name:   "vector?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(Vector)
			return Boolean(ok), nil
		},
	},
	{
		Name: "make-vector",
		Args: []string{"k", "[fill<any>]"},
		Return: &types.Type{
			Enum:    types.EnumVector,
			Element: types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			length, err := Int64(args[0])
			if err != nil || length < 0 {
				return nil, fmt.Errorf("invalid length: %v", args[0])
			}

			var fill Value
			if len(args) == 2 {
				fill = args[1]
			}
			elements := make([]Value, length, length)
			for i := 0; i < int(length); i++ {
				elements[i] = fill
			}

			return Vector(elements), nil
		},
	},
	{
		Name: "vector",
		Args: []string{"obj..."},
		Return: &types.Type{
			Enum:    types.EnumVector,
			Element: types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			v := make([]Value, len(args))
			copy(v, args)
			return Vector(v), nil
		},
	},
	{
		Name:   "vector-length",
		Args:   []string{"vector"},
		Return: types.InexactInteger,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			v, ok := args[0].(Vector)
			if !ok {
				return nil, fmt.Errorf("not a vector: %v", args[0])
			}
			return MakeNumber(len(v)), nil
		},
	},
	{
		Name:   "vector-ref",
		Args:   []string{"vector", "k"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			vector, ok := args[0].(Vector)
			if !ok {
				return nil, fmt.Errorf("invalid vector: %v", args[0])
			}
			k, err := Int64(args[1])
			if err != nil {
				return nil, fmt.Errorf("invalid index: %v", args[1])
			}
			if k < 0 || k >= int64(len(vector)) {
				return nil, fmt.Errorf("index %v out of range for vector %v",
					k, args[0])
			}
			return vector[k], nil
		},
	},
	{
		Name: "vector-set!",
		Args: []string{"vector", "k", "obj"},
		Return: &types.Type{
			Enum:    types.EnumVector,
			Element: types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			vector, ok := args[0].(Vector)
			if !ok {
				return nil, fmt.Errorf("invalid vector: %v", args[0])
			}
			k, error := Int64(args[1])
			if error != nil {
				return nil, fmt.Errorf("invalid index: %v", args[1])
			}
			if k < 0 || k >= int64(len(vector)) {
				return nil, fmt.Errorf("index %v out of range for vector %v",
					k, args[0])
			}
			vector[k] = args[2]
			return args[0], nil
		},
	},
	{
		Name: "vector->list",
		Args: []string{"vector"},
		Return: &types.Type{
			Enum: types.EnumPair,
			Car:  types.Any,
			Cdr:  types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			vector, ok := args[0].(Vector)
			if !ok {
				return nil, fmt.Errorf("invalid vector: %v", args[0])
			}
			var pair Pair

			for i := len(vector) - 1; i >= 0; i-- {
				pair = NewPair(vector[i], pair)
			}
			return pair, nil
		},
	},
	{
		Name: "list->vector",
		Args: []string{"list"},
		Return: &types.Type{
			Enum:    types.EnumVector,
			Element: types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var elements []Value
			err := Map(func(idx int, v Value) error {
				elements = append(elements, v)
				return nil
			}, args[0])
			if err != nil {
				return nil, err
			}
			return Vector(elements), nil
		},
	},
	{
		Name: "vector-fill!",
		Args: []string{"vector", "fill<any>"},
		Return: &types.Type{
			Enum:    types.EnumVector,
			Element: types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			vector, ok := args[0].(Vector)
			if !ok {
				return nil, fmt.Errorf("invalid vector: %v", args[0])
			}
			for i := 0; i < len(vector); i++ {
				vector[i] = args[1]
			}
			return args[0], nil
		},
	},
}

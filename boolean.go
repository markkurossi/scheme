//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

// Boolean implements boolean values.
type Boolean bool

// IsBoolean tests if the value is boolean.
func IsBoolean(value Value) (v bool, ok bool) {
	var b Boolean
	b, ok = value.(Boolean)
	if !ok {
		return
	}
	return bool(b), true
}

// Scheme returns the value as a Scheme string.
func (v Boolean) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v Boolean) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (v Boolean) Equal(o Value) bool {
	ov, ok := o.(Boolean)
	return ok && v == ov
}

func (v Boolean) String() string {
	return BooleanToScheme(bool(v))
}

// IsTrue tests if the argument value is true according to Scheme
// truth semantics i.e. the value is true unless it is boolean #f.
func IsTrue(v Value) bool {
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

// Equal tests if the values are equal?.
func Equal(a, b Value) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil && b != nil {
		return false
	}
	if a != nil && b == nil {
		return false
	}
	return a.Equal(b)
}

// Eq tests if the values are eq?.
func Eq(a, b Value) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil && b != nil {
		return false
	}
	if a != nil && b == nil {
		return false
	}
	return a.Eq(b)
}

var booleanBuiltins = []Builtin{
	{
		Name: "not",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Boolean(!IsTrue(args[0])), nil
		},
	},
	{
		Name: "boolean?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			_, ok := args[0].(Boolean)
			return Boolean(ok), nil
		},
	},
	{
		Name: "scheme::boolean=?",
		Args: []string{"bool1", "bool2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			bool1, ok := args[0].(Boolean)
			if !ok {
				return nil, l.Errorf("invalid boolean: %v", args[0])
			}
			bool2, ok := args[1].(Boolean)
			if !ok {
				return nil, l.Errorf("invalid boolean: %v", args[1])
			}
			return Boolean(bool1 == bool2), nil
		},
	},
	{
		Name: "eqv?",
		Args: []string{"obj1", "obj2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Boolean(Eq(args[0], args[1])), nil
		},
	},
	{
		Name: "eq?",
		Args: []string{"obj1", "obj2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Boolean(Eq(args[0], args[1])), nil
		},
	},
	{
		Name: "equal?",
		Args: []string{"obj1", "obj2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Boolean(Equal(args[0], args[1])), nil
		},
	},
}

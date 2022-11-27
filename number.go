//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

// Number implements numeric values.
type Number struct {
	Base  int
	Value interface{}
}

// NewNumber creates a new numeric value.
func NewNumber(base int, value interface{}) Number {
	var numValue interface{}

	switch v := value.(type) {
	case int:
		numValue = int64(v)

	case int64:
		numValue = v

	default:
		panic(fmt.Sprintf("unsupported number: %v(%T)", v, v))
	}
	return Number{
		Base:  base,
		Value: numValue,
	}
}

// Scheme returns the value as a Scheme string.
func (n Number) Scheme() string {
	return n.String()
}

// Equal tests if the argument value is equal to this number.
func (n Number) Equal(o Value) bool {
	on, ok := o.(Number)
	if !ok {
		return false
	}

	switch v := n.Value.(type) {
	case int64:
		switch ov := on.Value.(type) {
		case int64:
			return v == ov

		default:
			panic(fmt.Sprintf("uint64: o type %T not implemented", on.Value))
		}

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

func (n Number) String() string {
	switch v := n.Value.(type) {
	case int64:
		switch n.Base {
		case 2:
			return fmt.Sprintf("#b%b", v)
		case 8:
			return fmt.Sprintf("#o%o", v)
		case 10:
			return fmt.Sprintf("#d%d", v)
		case 16:
			return fmt.Sprintf("#x%x", v)

		default:
			return fmt.Sprintf("%v", n.Value)
		}

	default:
		return fmt.Sprintf("{%v[%T]}", n.Value, v)
	}
}

var numberBuiltins = []Builtin{
	{
		Name: "+",
		Args: []string{"[z1]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var sum int64
			for _, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("+: invalid argument %v", arg)
				}
				switch v := num.Value.(type) {
				case int64:
					sum += int64(v)
				default:
					return nil, fmt.Errorf("+: invalid agument %v", num)
				}
			}
			return NewNumber(0, sum), nil
		},
	},
	{
		Name: "*",
		Args: []string{"[z1]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var product int64 = 1
			for _, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("+: invalid argument %v", arg)
				}
				switch v := num.Value.(type) {
				case int64:
					product *= int64(v)
				default:
					return nil, fmt.Errorf("+: invalid agument %v", num)
				}
			}
			return NewNumber(0, product), nil
		},
	},
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

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
	return Number{
		Base:  base,
		Value: value,
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
	case int:
		switch ov := on.Value.(type) {
		case int:
			return v == ov

		default:
			panic(fmt.Sprintf("int: o type %T not implemented", on.Value))
		}

	case uint64:
		switch ov := on.Value.(type) {
		case int:
			return v == uint64(ov)

		case uint64:
			return v == v

		default:
			panic(fmt.Sprintf("uint64: o type %T not implemented", on.Value))
		}

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

func (n Number) String() string {
	switch v := n.Value.(type) {
	case uint64, int:
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

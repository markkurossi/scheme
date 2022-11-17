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

// Type returns the number value type.
func (n Number) Type() ValueType {
	return VNumber
}

// Scheme returns the value as a Scheme string.
func (n Number) Scheme() string {
	return n.String()
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

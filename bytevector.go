//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"bytes"
	"fmt"
	"strings"
)

// ByteVector implements bytevector values.
type ByteVector []byte

// Scheme returns the value as a Scheme string.
func (v ByteVector) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v ByteVector) Eq(o Value) bool {
	ov, ok := o.(ByteVector)
	if !ok {
		return false
	}
	if len(v) == 0 && len(ov) == 0 {
		return true
	}
	return false
}

// Equal tests if the argument value is equal to this value.
func (v ByteVector) Equal(o Value) bool {
	ov, ok := o.(ByteVector)
	if !ok || len(v) != len(ov) {
		return false
	}
	for idx, vv := range v {
		if vv != ov[idx] {
			return false
		}
	}
	return true
}

func (v ByteVector) String() string {
	var str strings.Builder
	str.WriteString("#vu8(")

	for idx, el := range v {
		if idx > 0 {
			str.WriteRune(' ')
		}
		str.WriteString(fmt.Sprintf("%d", el))
	}
	str.WriteRune(')')
	return str.String()
}

var bytevectorBuiltins = []Builtin{
	{
		Name: "bytevector?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			_, ok := args[0].(ByteVector)
			return Boolean(ok), nil
		},
	},
	// XXX make-bytevector
	{
		Name: "bytevector-length",
		Args: []string{"bytevector"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			v, ok := args[0].(ByteVector)
			if !ok {
				return nil, l.Errorf("not a bytevector: %v", args[0])
			}
			return NewNumber(0, len(v)), nil
		},
	},
	{
		Name: "bytevector=?",
		Args: []string{"bytevector1", "bytevector2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			v1, ok := args[0].(ByteVector)
			if !ok {
				return nil, l.Errorf("not a bytevector: %v", args[0])
			}
			v2, ok := args[1].(ByteVector)
			if !ok {
				return nil, l.Errorf("not a bytevector: %v", args[1])
			}
			return Boolean(bytes.Equal(v1, v2)), nil
		},
	},
}

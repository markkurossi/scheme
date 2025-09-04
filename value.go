//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

var (
	_ Value = &Number{}
	_ Value = &BigInt{}
	_ Value = &BigFloat{}
	_ Value = Int(0)
	_ Value = Float(0.0)
	_ Value = &Bytevector{}
	_ Value = &Frame{}
	_ Value = &Symbol{}
	_ Value = &Lambda{}
	_ Value = &PlainPair{}
	_ Value = &Port{}
	_ Value = &EOFObject{}
	_ Value = &Vector{}
	_ Value = Boolean(true)
	_ Value = Character('@')
	_ Value = String("string")
	_ Value = &Error{}
)

// Value implements a Scheme value.
type Value interface {
	Scheme() string
	Eq(o Value) bool
	Equal(o Value) bool
	Type() *types.Type
	Unbox() (Value, *types.Type)
}

// ToString returns a display representation of the value.
func ToString(v Value) string {
	if v == nil {
		return "'()"
	}
	return fmt.Sprintf("%v", v)
}

// ToScheme returns a Scheme representation of the value.
func ToScheme(v Value) string {
	if v == nil {
		return "'()"
	}
	return v.Scheme()
}

// Unbox returns the value and its type from the boxed value v.
func Unbox(v Value) (Value, *types.Type) {
	if v == nil {
		return v, types.Nil
	}
	return v.Unbox()
}

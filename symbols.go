//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"

	"github.com/markkurossi/scheme/types"
)

// Flags define symbol flags.
type Flags int

// Symbol flags.
const (
	FlagDefined Flags = 1 << iota
	FlagConst
)

func (f Flags) String() string {
	var result string
	if f&FlagDefined != 0 {
		result += " defined"
	}
	if f&FlagConst != 0 {
		result += " const"
	}
	return strings.TrimSpace(result)
}

// Symbol implements symbol values.
type Symbol struct {
	Name       string
	Point      Point
	GlobalType *types.Type
	Global     Value
	Flags      Flags
}

// NewSymbol creates a new symbol for the name.
func NewSymbol(name string) *Symbol {
	return &Symbol{
		Name: name,
	}
}

// Scheme returns the value as a Scheme string.
func (v *Symbol) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v *Symbol) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (v *Symbol) Equal(o Value) bool {
	ov, ok := o.(*Symbol)
	return ok && v.Name == ov.Name
}

// Type implements Value.Type.
func (v *Symbol) Type() *types.Type {
	return types.Symbol
}

// Unbox implements Value.Unbox.
func (v *Symbol) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

func (v *Symbol) String() string {
	return v.Name
}

// Symbolp tests if the value is a symbol.
func Symbolp(v Value) bool {
	_, ok := v.(*Symbol)
	return ok
}

// IsSymbol tests if the value is the symbol sym.
func IsSymbol(v Value, sym string) bool {
	id, ok := v.(*Symbol)
	return ok && id.Name == sym
}

var symbolBuiltins = []Builtin{
	{
		Name:   "symbol?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return Boolean(Symbolp(args[0])), nil
		},
	},
	{
		Name:   "symbol->string",
		Args:   []string{"symbol"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			id, ok := args[0].(*Symbol)
			if !ok {
				return nil, fmt.Errorf("not a symbol: %v", args[0])
			}
			return String(id.Name), nil
		},
	},
	{
		Name:   "symbol=?",
		Args:   []string{"symbol1", "symbol2", "symbol3..."},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var last *Symbol

			for idx, arg := range args {
				id, ok := arg.(*Symbol)
				if !ok {
					return nil, fmt.Errorf("invalid symbol: %v", arg)
				}
				if idx > 0 && id.Name != last.Name {
					return Boolean(false), nil
				}
				last = id
			}
			return Boolean(true), nil
		},
	},
	{
		Name:   "string->symbol",
		Args:   []string{"string"},
		Return: types.Symbol,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("not a string: %v", args[0])
			}
			return NewSymbol(string(str)), nil
		},
	},
}

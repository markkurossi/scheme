//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

// Symbolp tests if the value is a symbol.
func Symbolp(v Value) bool {
	_, ok := v.(*Identifier)
	return ok
}

// IsSymbol tests if the value is the symbol sym.
func IsSymbol(v Value, sym string) bool {
	id, ok := v.(*Identifier)
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
			id, ok := args[0].(*Identifier)
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
			var last *Identifier

			for idx, arg := range args {
				id, ok := arg.(*Identifier)
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
			return &Identifier{
				Name: string(str),
			}, nil
		},
	},
}

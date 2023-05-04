//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

var symbolBuiltins = []Builtin{
	{
		Name: "symbol?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(*Identifier)
			return Boolean(ok), nil
		},
	},
	{
		Name: "symbol->string",
		Args: []string{"symbol"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			id, ok := args[0].(*Identifier)
			if !ok {
				return nil, fmt.Errorf("not a symbol: %v", args[0])
			}
			return String(id.Name), nil
		},
	},
	{
		Name: "symbol=?",
		Args: []string{"symbol1", "symbol2", "symbol3..."},
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
		Name: "string->symbol",
		Args: []string{"string"},
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

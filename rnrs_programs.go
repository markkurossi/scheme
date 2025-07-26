//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//
// The (rnrs programs (6)) library.
//

package scheme

import (
	"fmt"
	"os"

	"github.com/markkurossi/scheme/types"
)

var rnrsProgramsBuiltins = []Builtin{
	{
		Name: "command-line",
		Return: &types.Type{
			Enum: types.EnumPair,
			Car:  types.String,
			Cdr:  types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var result ListBuilder
			for _, arg := range os.Args {
				result.Add(String(arg))
			}
			return result.Head, nil
		},
	},
	{
		Name:   "exit",
		Args:   []string{"[obj]"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			code := 0
			if len(args) == 1 {
				switch v := args[0].(type) {
				case Int:
					code = int(v)

				case *BigInt:
					code = int(v.I.Int64())

				case Boolean:
					if !v {
						code = 1
					}

				default:
					code = 1
				}
			}
			os.Exit(code)
			return nil, nil
		},
	},
	{
		Name:   "getenv",
		Args:   []string{"name<string>"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			name, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid variable name: %v", args[0])
			}
			val, found := os.LookupEnv(string(name))
			if !found {
				return Boolean(false), nil
			}
			return String(val), nil
		},
	},
}

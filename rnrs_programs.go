//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//
// The (rnrs programs (6)) library.
//

package scheme

import (
	"os"
)

var rnrsProgramsBuiltins = []Builtin{
	{
		Name: "command-line",
		Args: []string{"[obj]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var head, tail Pair

			for _, arg := range os.Args {
				pair := NewPair(String(arg), nil)
				if tail == nil {
					head = pair
				} else {
					tail.SetCdr(pair)
				}
				tail = pair
			}
			return head, nil
		},
	},
	{
		Name: "exit",
		Args: []string{"[obj]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
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
		Name: "getenv",
		Args: []string{"name"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			name, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid variable name: %v", args[0])
			}
			val, found := os.LookupEnv(string(name))
			if !found {
				return Boolean(false), nil
			}
			return String(val), nil
		},
	},
}

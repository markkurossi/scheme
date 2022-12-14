//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"os"
)

var osBuiltins = []Builtin{
	{
		Name: "exit",
		Args: []string{"[obj]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			code := 0
			if len(args) == 1 {
				switch v := args[0].(type) {
				case Number:
					code = int(v.Int64())

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
			val, _ := os.LookupEnv(string(name))
			return String(val), nil
		},
	},
}

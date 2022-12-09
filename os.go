//
// Copyright (c) 2022 Markku Rossi
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
}

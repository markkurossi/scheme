//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

var debugBuiltins = []Builtin{
	{
		Name: "disassemble",
		Args: []string{"value"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch arg := args[0].(type) {
			case *Lambda:
				fmt.Fprintf(scm.Stdout, "lambda: %v\n", arg)
				if arg.Native == nil {
					for _, c := range arg.Code {
						fmt.Printf("%s\n", c)
					}
				}

			default:
				fmt.Fprintf(scm.Stdout, "value: %v\n", arg)
			}
			return nil, nil
		},
	},
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
)

var outputBuiltins = []Builtin{
	{
		Name: "display",
		Args: []string{"obj", "[port]"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			for idx, arg := range args {
				if idx > 0 {
					fmt.Print(" ")
				}
				fmt.Printf("%v", arg)
			}
			return nil, nil
		},
	},
	{
		Name: "newline",
		Args: []string{"[port]"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			fmt.Println()
			return nil, nil
		},
	},
}

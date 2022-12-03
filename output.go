//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

var outputBuiltins = []Builtin{
	{
		Name: "display",
		Args: []string{"obj", "[port]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			for idx, arg := range args {
				if idx > 0 {
					fmt.Fprint(scm.Stdout, " ")
				}
				fmt.Fprintf(scm.Stdout, "%v", arg)
			}
			return nil, nil
		},
	},
	{
		Name: "newline",
		Args: []string{"[port]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			fmt.Fprintln(scm.Stdout)
			return nil, nil
		},
	},
}

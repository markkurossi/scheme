//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"sort"
)

var debugBuiltins = []Builtin{
	{
		Name: "print-env",
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var names []string
			for k, v := range scm.symbols {
				if v.Global != nil {
					names = append(names, k)
				}
			}
			sort.Strings(names)
			fmt.Printf("Global symbols:\n")
			for _, name := range names {
				fmt.Printf("%16s : %s\n", name, scm.symbols[name].Global)
			}
			return nil, nil
		},
	},
	{
		Name: "disassemble",
		Args: []string{"value"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch arg := args[0].(type) {
			case *Lambda:
				scm.Stdout.Printf("lambda: %v\n", arg)
				if arg.Native == nil {
					for _, c := range arg.Code {
						scm.Stdout.Printf("%s\n", c)
					}
				}

			default:
				scm.Stdout.Printf("value: %v\n", arg)
			}
			return nil, nil
		},
	},
}

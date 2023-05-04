//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

var procedureBuiltins = []Builtin{
	{
		Name: "procedure?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch args[0].(type) {
			case *Lambda:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
}

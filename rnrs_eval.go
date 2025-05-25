//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

var rnrsEvalBuiltins = []Builtin{
	{
		Name:   "eval",
		Args:   []string{"obj", "obj"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case *BigInt, *Bytevector, *Port, *Vector, Boolean, Character,
				Float, Int, String:
				return v, nil

			case *Identifier:
				id, ok := scm.symbols[v.Name]
				if !ok {
					return nil, fmt.Errorf("undefined symbol '%v'", v.Name)
				}
				return id.Global, nil

			default:
				return nil, fmt.Errorf("can't eval %T", args[0])
			}
		},
	},
}

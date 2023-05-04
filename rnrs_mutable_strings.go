//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//
// The (rnrs mutable-strings (6)) library.
//

package scheme

import (
	"fmt"
)

var rnrsMutableStringsBuiltins = []Builtin{
	{
		Name: "string-set!",
		Args: []string{"string", "k", "char"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			k, err := Int64(args[1])
			if err != nil {
				return nil, fmt.Errorf("invalid index: %v", args[1])
			}
			_ = str
			_ = k
			return nil, fmt.Errorf("not implemented yet")
		},
	},
	// XXX string-fill!
}

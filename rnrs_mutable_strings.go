//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//
// The (rnrs mutable-strings (6)) library.
//

package scheme

var rnrsMutableStringsBuiltins = []Builtin{
	{
		Name: "string-set!",
		Args: []string{"string", "k", "char"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid string: %v", args[0])
			}
			kn, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid index: %v", args[1])
			}
			k := kn.Int64()
			_ = str
			_ = k
			return nil, l.Errorf("not implemented yet")
		},
	},
	// XXX string-fill!
}

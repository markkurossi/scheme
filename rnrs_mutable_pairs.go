//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//
// The (rnrs mutable-pairs (6)) library.
//

package scheme

var rnrsMutablePairsBuiltins = []Builtin{
	{
		Name: "set-car!",
		Args: []string{"pair", "obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			pair, ok := args[0].(Pair)
			if !ok {
				return nil, l.Errorf("not a pair: %v", args[0])
			}
			err := pair.SetCar(args[1])
			if err != nil {
				return nil, err
			}
			return nil, nil
		},
	},
	{
		Name: "set-cdr!",
		Args: []string{"pair", "obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			pair, ok := args[0].(Pair)
			if !ok {
				return nil, l.Errorf("not a pair: %v", args[0])
			}
			err := pair.SetCdr(args[1])
			if err != nil {
				return nil, err
			}
			return nil, nil
		},
	},
}

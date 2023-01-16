//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"sort"
)

const (
	fLambda int = 1 << iota
	fNative
	fScheme
)

var debugBuiltins = []Builtin{
	{
		Name: "print-env",
		Args: []string{"flags..."},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {

			var flags int

			if len(args) == 0 {
				flags = 0xffff
			} else {
				for _, arg := range args {
					switch flag := arg.(type) {
					case *Identifier:
						switch flag.Name {
						case "native":
							flags |= fNative
						case "scheme":
							flags |= fScheme

						default:
							return nil, l.Errorf("unknown flag: %v", arg)
						}

					case Keyword:
						switch flag {
						case KwLambda:
							flags |= fLambda

						default:
							return nil, l.Errorf("unknown flag: %v", arg)
						}

					default:
						return nil, l.Errorf("invalid flag: %v(%T)", arg, arg)
					}
				}
			}
			var names []string
			for k, v := range scm.symbols {
				if v.Global != nil {
					switch val := v.Global.(type) {
					case *Lambda:
						if flags&fLambda != 0 {
							if (flags&(fNative|fScheme)) == 0 ||
								((val.Native != nil) && (flags&fNative != 0)) ||
								((val.Native == nil) && (flags&fScheme != 0)) {
								names = append(names, k)
							}
						}

					default:
						names = append(names, k)
					}
				}
			}
			sort.Strings(names)
			fmt.Printf("Global symbols:\n")
			for _, name := range names {
				fmt.Printf("%19s : %s\n", name, scm.symbols[name].Global)
			}
			fmt.Printf("%d symbols matched\n", len(names))
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

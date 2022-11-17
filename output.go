//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"math"
)

var outputBuiltins = []Builtin{
	{
		Name:    "display",
		MinArgs: 1,
		MaxArgs: math.MaxInt,
		Native: func(vm *VM, args []Value) (Value, error) {
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
		Native: func(vm *VM, args []Value) (Value, error) {
			fmt.Println()
			return nil, nil
		},
	},
}

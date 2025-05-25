//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//
// The (rnrs files (6)) library.
//

package scheme

import (
	"errors"
	"fmt"
	"os"

	"github.com/markkurossi/scheme/types"
)

var rnrsFilesBuiltins = []Builtin{
	{
		Name:   "file-exists?",
		Args:   []string{"filename<string>"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			filename, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid filename: %v", args[0])
			}
			_, err := os.Stat(string(filename))
			return Boolean(!errors.Is(err, os.ErrNotExist)), nil
		},
	},
	{
		Name:   "delete-file",
		Args:   []string{"filename<string>"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			filename, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid filename: %v", args[0])
			}
			err := os.Remove(string(filename))
			if err != nil {
				return nil, err
			}
			return Boolean(true), nil
		},
	},

	{
		Name:   "current-directory",
		Args:   []string{"[path<string>]"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			if len(args) == 1 {
				d, ok := args[0].(String)
				if !ok {
					return nil, fmt.Errorf("invalid directory: %v", args[0])
				}
				err := os.Chdir(string(d))
				if err != nil {
					return nil, err
				}
				return d, nil
			}
			d, err := os.Getwd()
			if err != nil {
				return nil, err
			}
			return String(d), nil
		},
	},
	{
		Name: "directory-list",
		Args: []string{"[path<string>]"},
		Return: &types.Type{
			Enum: types.EnumPair,
			Car:  types.String,
			Cdr:  types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			dir := "."
			if len(args) == 1 {
				d, ok := args[0].(String)
				if !ok {
					return nil, fmt.Errorf("invalid path: %v", args[0])
				}
				dir = string(d)
			}
			f, err := os.Open(dir)
			if err != nil {
				return nil, err
			}
			defer f.Close()

			names, err := f.Readdirnames(0)
			if err != nil {
				return nil, err
			}

			var result Pair
			for i := len(names) - 1; i >= 0; i-- {
				result = NewPair(String(names[i]), result)
			}

			return result, nil
		},
	},
}

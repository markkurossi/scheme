//
// Copyright (c) 2023 Markku Rossi
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
}

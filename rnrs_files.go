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
	"os"
)

var rnrsFilesBuiltins = []Builtin{
	{
		Name: "file-exists?",
		Args: []string{"string:filename"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			filename, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid filename: %v", args[0])
			}
			_, err := os.Stat(string(filename))
			return Boolean(!errors.Is(err, os.ErrNotExist)), nil
		},
	},
	{
		Name: "delete-file",
		Args: []string{"string:filename"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			filename, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid filename: %v", args[0])
			}
			err := os.Remove(string(filename))
			if err != nil {
				return nil, err
			}
			return Boolean(true), nil
		},
	},
}

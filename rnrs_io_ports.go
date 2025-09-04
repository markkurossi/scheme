//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"

	"github.com/markkurossi/scheme/types"
)

var rnrsIOPortsBuiltins = []Builtin{
	{
		Name:   "get-line",
		Args:   []string{"port"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return nil, fmt.Errorf("invalid port: %v", args[0])
			}
			var result []byte
			var buf [1]byte
			for {
				_, err := port.Read(buf[:])
				if err != nil {
					if err == io.EOF {
						return EOF, nil
					}
					return nil, err
				}
				if buf[0] == '\n' {
					break
				}
				result = append(result, buf[0])
			}
			return String(result), nil
		},
	},
}

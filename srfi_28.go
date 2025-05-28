//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//
// SRFI 28: Basic Format Strings
//  - https://srfi.schemers.org/srfi-28/srfi-28.html
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

var srfi28Builtins = []Builtin{
	{
		Name:   "format",
		Args:   []string{"format<string>", "obj..."},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			formatString, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			format := []rune(string(formatString))
			var result []rune
			argIdx := 1

			for i := 0; i < len(format); i++ {
				if format[i] != '~' || i+1 >= len(format) {
					result = append(result, format[i])
					continue
				}
				i++
				var fragment string
				switch format[i] {
				case 'a':
					if argIdx >= len(args) {
						return nil,
							fmt.Errorf("No value for escape sequence ~a")
					}
					fragment = ToString(args[argIdx])
					argIdx++
				case 's':
					if argIdx >= len(args) {
						return nil,
							fmt.Errorf("No value for escape sequence ~s")
					}
					fragment = ToScheme(args[argIdx])
					argIdx++
				case '%':
					fragment = "\n"
				case '~':
					fragment = "~"
				default:
					return nil, fmt.Errorf("Unrecognized escape sequence ~%c",
						format[i])
				}
				result = append(result, []rune(fragment)...)
			}

			return String(string(result)), nil
		},
	},
}

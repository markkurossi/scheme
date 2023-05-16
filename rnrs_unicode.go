//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/markkurossi/scheme/types"
)

// rnrs unicode (6)

var rnrsUnicodeBuiltins = []Builtin{
	// 1.1. Characters
	{
		Name:   "char-upcase",
		Args:   []string{"char"},
		Return: types.Character,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Character(unicode.ToUpper(rune(ch))), nil
		},
	},
	{
		Name:   "char-downcase",
		Args:   []string{"char"},
		Return: types.Character,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Character(unicode.ToLower(rune(ch))), nil
		},
	},
	{
		Name:   "char-titlecase",
		Args:   []string{"char"},
		Return: types.Character,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Character(unicode.ToTitle(rune(ch))), nil
		},
	},
	// XXX char-foldcase
	{
		Name:   "char-alphabetic?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsLetter(rune(ch))), nil
		},
	},
	{
		Name:   "char-numeric?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsDigit(rune(ch))), nil
		},
	},
	{
		Name:   "char-whitespace?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsSpace(rune(ch))), nil
		},
	},
	{
		Name:   "char-upper-case?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsUpper(rune(ch))), nil
		},
	},
	{
		Name:   "char-lower-case?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsLower(rune(ch))), nil
		},
	},
	{
		Name:   "char-title-case?",
		Args:   []string{"char"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, fmt.Errorf("invalid character: %v", args[0])
			}
			return Boolean(unicode.IsTitle(rune(ch))), nil
		},
	},
	// XXX char-general-category

	// 1.2. Strings
	{
		Name:   "string-upcase",
		Args:   []string{"string"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			return String(strings.ToUpper(string(str))), nil
		},
	},
	{
		Name:   "string-downcase",
		Args:   []string{"string"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			return String(strings.ToLower(string(str))), nil
		},
	},
	{
		Name:   "string-titlecase",
		Args:   []string{"string"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			// Deprecated: The rule Title uses for word boundaries
			// does not handle Unicode punctuation properly. Use
			// golang.org/x/text/cases instead.
			return String(strings.Title(strings.ToLower(string(str)))), nil
		},
	},
	// XXX string-foldcase
	// XXX string-normalize-nfd
	// XXX string-normalize-nfkd
	// XXX string-normalize-nfc
	// XXX string-normalize-nfkc
}

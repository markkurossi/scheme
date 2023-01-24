//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"unicode"
)

// rnrs unicode (6)

var rnrsUnicodeBuiltins = []Builtin{
	{
		Name: "char-upcase",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Character(unicode.ToUpper(rune(ch))), nil
		},
	},
	{
		Name: "char-downcase",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Character(unicode.ToLower(rune(ch))), nil
		},
	},
	{
		Name: "char-titlecase",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Character(unicode.ToTitle(rune(ch))), nil
		},
	},
	// XXX char-foldcase
	{
		Name: "char-alphabetic?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsLetter(rune(ch))), nil
		},
	},
	{
		Name: "char-numeric?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsDigit(rune(ch))), nil
		},
	},
	{
		Name: "char-whitespace?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsSpace(rune(ch))), nil
		},
	},
	{
		Name: "char-upper-case?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsUpper(rune(ch))), nil
		},
	},
	{
		Name: "char-lower-case?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsLower(rune(ch))), nil
		},
	},
	{
		Name: "char-title-case?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			ch, ok := args[0].(Character)
			if !ok {
				return nil, l.Errorf("invalid character %v", args[0])
			}
			return Boolean(unicode.IsTitle(rune(ch))), nil
		},
	},
}

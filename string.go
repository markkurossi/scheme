//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"strings"
)

// String implements string values.
type String []byte

// Scheme returns the value as a Scheme string.
func (v String) Scheme() string {
	return StringToScheme(string(v))
}

// Eq tests if the argument value is eq? to this value.
func (v String) Eq(o Value) bool {
	ov, ok := o.(String)
	if !ok {
		return false
	}
	if len(v) == 0 && len(ov) == 0 {
		return true
	}
	return false
}

// Equal tests if the argument value is equal to this value.
func (v String) Equal(o Value) bool {
	ov, ok := o.(String)
	if !ok || len(v) != len(ov) {
		return false
	}
	for i := 0; i < len(v); i++ {
		if v[i] != ov[i] {
			return false
		}
	}
	return true
}

func (v String) String() string {
	return string(v)
}

// StringToScheme returns the string as Scheme string literal.
func StringToScheme(s string) string {
	var str strings.Builder
	str.WriteRune('"')
	for _, r := range s {
		switch r {
		case '\\', '"', '|', '(':
			str.WriteRune('\\')
			str.WriteRune(r)
		case '\a':
			str.WriteRune('\\')
			str.WriteRune('a')
		case '\f':
			str.WriteRune('\\')
			str.WriteRune('f')
		case '\n':
			str.WriteRune('\\')
			str.WriteRune('n')
		case '\r':
			str.WriteRune('\\')
			str.WriteRune('r')
		case '\t':
			str.WriteRune('\\')
			str.WriteRune('t')
		case '\v':
			str.WriteRune('\\')
			str.WriteRune('v')
		case '\b':
			str.WriteRune('\\')
			str.WriteRune('b')
		case 0:
			str.WriteRune('\\')
			str.WriteRune('0')
		default:
			str.WriteRune(r)
		}
	}
	str.WriteRune('"')
	return str.String()
}

var stringBuiltins = []Builtin{
	{
		Name: "string?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			_, ok := args[0].(String)
			return Boolean(ok), nil
		},
	},
	{
		Name: "make-string",
		Args: []string{"k", "[char]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			k, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid length: %v", args[0])
			}
			length := k.Int64()
			if length < 0 {
				return nil, l.Errorf("negative length: %v", k)
			}

			fill := byte(' ')
			if len(args) == 2 {
				ch, ok := args[1].(Character)
				if !ok {
					return nil, l.Errorf("invalid char: %v", args[1])
				}
				fill = byte(ch)
			}
			str := make([]byte, length, length)
			for i := 0; i < int(length); i++ {
				str[i] = fill
			}

			return String(str), nil
		},
	},
	{
		Name: "string",
		Args: []string{"char..."},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			length := len(args)
			str := make([]byte, length, length)

			for i := 0; i < length; i++ {
				ch, ok := args[i].(Character)
				if !ok {
					return nil, l.Errorf("invalid character: %v", args[i])
				}
				str[i] = byte(ch)
			}
			return String(str), nil
		},
	},
	{
		Name: "string-length",
		Args: []string{"string"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case String:
				return NewNumber(0, len(v)), nil

			default:
				return nil, l.Errorf("invalid argument")
			}
		},
	},
	{
		Name: "string-ref",
		Args: []string{"string", "k"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid string: %v", args[0])
			}
			kn, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid index: %v", args[1])
			}
			k := kn.Int64()
			if k < 0 || k >= int64(len(str)) {
				return nil, l.Errorf("invalid index: %v", args[1])
			}
			return Character(str[k]), nil
		},
	},
	// XXX string-set!
	// XXX string=?
	// XXX string-ci=?
	// XXX string<?
	// XXX string>?
	// XXX string<=?
	// XXX string>=?
	// XXX string-ci<?
	// XXX string-ci>?
	// XXX string-ci<=?
	// XXX string-ci>=?
	// XXX substring
	{
		Name: "string-append",
		Args: []string{"string..."},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var str []byte

			for i := 0; i < len(args); i++ {
				s, ok := args[i].(String)
				if !ok {
					return nil, l.Errorf("invalid string: %v", args[i])
				}
				str = append(str, s...)
			}
			return String(str), nil
		},
	},
	{
		Name: "string->list",
		Args: []string{"string"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid string: %v", args[0])
			}
			runes := []rune(string(str))

			var head, tail Pair
			for i := 0; i < len(runes); i++ {
				item := NewPair(Character(runes[i]), nil)
				if head == nil {
					head = item
				} else {
					tail.SetCdr(item)
				}
				tail = item
			}
			return head, nil
		},
	},
	{
		Name: "list->string",
		Args: []string{"chars"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var str []byte
			err := Map(func(idx int, v Value) error {
				ch, ok := v.(Character)
				if !ok {
					return l.Errorf("invalid character: %v", v)
				}
				str = append(str, byte(ch))
				return nil
			}, args[0])
			if err != nil {
				return nil, err
			}
			return String(str), nil
		},
	},
	{
		Name: "string-copy",
		Args: []string{"string"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid string: %v", args[0])
			}
			new := make([]byte, len(str), len(str))
			copy(new, str)
			return String(new), nil
		},
	},
	// XXX string-fill!
}

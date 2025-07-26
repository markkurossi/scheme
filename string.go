//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"

	"github.com/markkurossi/scheme/types"
)

// String implements string values.
type String string

// IsString tests if the value is string.
func IsString(value Value) (v string, ok bool) {
	var str String
	str, ok = value.(String)
	if !ok {
		return
	}
	return string(str), true
}

// Scheme returns the value as a Scheme string.
func (v String) Scheme() string {
	return StringToScheme(string(v))
}

// Eq tests if the argument value is eq? to this value.
func (v String) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (v String) Equal(o Value) bool {
	ov, ok := o.(String)
	return ok && v == ov
}

// Type implements the Value.Type().
func (v String) Type() *types.Type {
	return types.String
}

// Unbox implements Value.Unbox.
func (v String) Unbox() (Value, *types.Type) {
	return v, v.Type()
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
		Name:   "string?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(String)
			return Boolean(ok), nil
		},
	},
	{
		Name:   "make-string",
		Args:   []string{"k", "[char]"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			length, err := Int64(args[0])
			if err != nil || length < 0 {
				return nil, fmt.Errorf("invalid length: %v", args[0])
			}

			fill := ' '
			if len(args) == 2 {
				ch, ok := args[1].(Character)
				if !ok {
					return nil, fmt.Errorf("invalid char: %v", args[1])
				}
				fill = rune(ch)
			}
			str := make([]rune, length, length)
			for i := 0; i < int(length); i++ {
				str[i] = fill
			}

			return String(string(str)), nil
		},
	},
	{
		Name:   "string",
		Args:   []string{"char..."},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			length := len(args)
			str := make([]byte, length, length)

			for i := 0; i < length; i++ {
				ch, ok := args[i].(Character)
				if !ok {
					return nil, fmt.Errorf("invalid character: %v", args[i])
				}
				str[i] = byte(ch)
			}
			return String(str), nil
		},
	},
	{
		Name:   "string-length",
		Args:   []string{"string"},
		Return: types.InexactInteger,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			switch v := args[0].(type) {
			case String:
				return MakeNumber(len([]rune(string(v)))), nil

			default:
				return nil, fmt.Errorf("invalid argument: %v", args[0])
			}
		},
	},
	{
		Name:   "string-ref",
		Args:   []string{"string", "k"},
		Return: types.Character,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			chars := []rune(string(str))

			k, err := Int64(args[1])
			if err != nil || k < 0 || k >= int64(len(chars)) {
				return nil, fmt.Errorf("invalid index: %v", args[1])
			}
			return Character(chars[k]), nil
		},
	},
	{
		Name:   "scheme::string=?",
		Args:   []string{"string1", "string2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str1, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str2, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[1])
			}
			return Boolean(string(str1) == string(str2)), nil
		},
	},
	{
		Name:   "scheme::string<?",
		Args:   []string{"string1", "string2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str1, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str2, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[1])
			}
			return Boolean(string(str1) < string(str2)), nil
		},
	},
	{
		Name:   "scheme::string>?",
		Args:   []string{"string1", "string2"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str1, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str2, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[1])
			}
			return Boolean(string(str1) > string(str2)), nil
		},
	},
	{
		Name:   "substring",
		Args:   []string{"string", "start", "end"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			strv, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str := []rune(string(strv))

			start, err := Int64(args[1])
			if err != nil {
				return nil, fmt.Errorf("invalid start index: %v", args[1])
			}

			end, err := Int64(args[2])
			if err != nil {
				return nil, fmt.Errorf("invalid end index: %v", args[2])
			}

			if start < 0 || end < start || end > int64(len(strv)) {
				return nil, fmt.Errorf("invalid indices: 0 <= %v <= %v <= %v",
					start, end, len(strv))
			}

			return String(string(str[start:end])), nil
		},
	},
	{
		Name:   "string-append",
		Args:   []string{"string..."},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var result string

			for _, arg := range args {
				str, ok := arg.(String)
				if !ok {
					return nil, fmt.Errorf("invalid string: %v", arg)
				}
				result += string(str)
			}
			return String(result), nil
		},
	},
	{
		Name: "string->list",
		Args: []string{"string"},
		Return: &types.Type{
			Enum: types.EnumPair,
			Car:  types.Character,
			Cdr:  types.Any,
		},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			runes := []rune(string(str))

			var result ListBuilder
			for i := 0; i < len(runes); i++ {
				result.Add(Character(runes[i]))
			}
			return result.Head, nil
		},
	},
	{
		Name:   "list->string",
		Args:   []string{"chars"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var str []rune
			err := Map(func(idx int, v Value) error {
				ch, ok := v.(Character)
				if !ok {
					return fmt.Errorf("invalid character: %v", v)
				}
				str = append(str, rune(ch))
				return nil
			}, args[0])
			if err != nil {
				return nil, err
			}
			return String(string(str)), nil
		},
	},
	{
		Name:   "string-copy",
		Args:   []string{"string"},
		Return: types.String,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			str, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			runes := []rune(string(str))
			new := make([]rune, len(runes), len(runes))
			copy(new, runes)
			return String(string(new)), nil
		},
	},

	{
		Name:   "string-prefix?",
		Args:   []string{"prefix<string>", "string"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			prefix, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[1])
			}
			return Boolean(strings.HasPrefix(string(str), string(prefix))), nil
		},
	},
	{
		Name:   "string-suffix?",
		Args:   []string{"suffix<string>", "string"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			prefix, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[0])
			}
			str, ok := args[1].(String)
			if !ok {
				return nil, fmt.Errorf("invalid string: %v", args[1])
			}
			return Boolean(strings.HasSuffix(string(str), string(prefix))), nil
		},
	},
}

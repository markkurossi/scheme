//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"strconv"
	"unicode"
)

// Character implements character values.
type Character rune

// Scheme returns the value as a Scheme string.
func (v Character) Scheme() string {
	return CharacterToScheme(rune(v))
}

// Equal tests if the argument value is equal to this value.
func (v Character) Equal(o Value) bool {
	ch, ok := o.(Character)
	return ok && v == ch
}

func (v Character) String() string {
	return fmt.Sprintf("%c", v)
}

var characters = map[string]rune{
	"alarm":     0x0007,
	"backspace": 0x0008,
	"tab":       0x0009,
	"newline":   0x000A,
	"linefeed":  0x000A,
	"vtab":      0x000B,
	"page":      0x000C,
	"return":    0x000D,
	"space":     0x0020,
	"delete":    0x007F,

	// C0 control character names.
	"nul": 0,
	"soh": 1,
	"stx": 2,
	"etx": 3,
	"eot": 4,
	"enq": 5,
	"ack": 6,
	"bel": 7,
	"bs":  8,
	"ht":  9,
	"lf":  10,
	"vt":  11,
	"ff":  12,
	"cr":  13,
	"so":  14,
	"si":  15,
	"dle": 16,
	"dc1": 17,
	"dc2": 18,
	"dc3": 19,
	"dc4": 20,
	"nak": 21,
	"syn": 22,
	"etb": 23,
	"can": 24,
	"em":  25,
	"sub": 26,
	"esc": 27,
	"fs":  28,
	"gs":  29,
	"rs":  30,
	"us":  31,
	"sp":  32,
}

func lookupCharacter(name []rune) (rune, error) {
	if len(name) == 0 {
		return 0, fmt.Errorf("invalid character name")
	}
	if len(name) == 1 {
		return name[0], nil
	}
	str := string(name)
	ch, ok := characters[str]
	if ok {
		return ch, nil
	}
	// #\xhhhh
	if name[0] == 'x' {
		uval, err := strconv.ParseUint(str[1:], 16, 64)
		if err == nil {
			return rune(uval), nil
		}
	}
	// #\ooo
	valid8 := true
	for i := 0; i < len(name); i++ {
		if !IsDigit8(name[i]) {
			valid8 = false
			break
		}
	}
	if valid8 {
		uval, err := strconv.ParseUint(str, 8, 64)
		if err == nil {
			return rune(uval), nil
		}
	}

	return 0, fmt.Errorf("unknown character '%s'", str)
}

// CharacterToScheme returns the rune as Scheme character literal.
func CharacterToScheme(r rune) string {
	for name, ch := range characters {
		if r == ch {
			return fmt.Sprintf("#\\%s", name)
		}
	}
	if unicode.IsPrint(r) {
		return fmt.Sprintf("#\\%c", r)
	}
	return fmt.Sprintf("#\\x%x", r)
}

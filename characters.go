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

var characters = map[string]rune{
	"nul":       0x0000,
	"alarm":     0x0007,
	"backspace": 0x0008,
	"tab":       0x0009,
	"newline":   0x000A,
	"linefeed":  0x000A,
	"vtab":      0x000B,
	"page":      0x000C,
	"return":    0x000D,
	"esc":       0x001B,
	"space":     0x0020,
	"delete":    0x007F,
}

func lookupCharacter(name []rune) (rune, error) {
	if len(name) == 1 {
		return name[0], nil
	}
	str := string(name)
	ch, ok := characters[str]
	if ok {
		return ch, nil
	}
	if name[0] == 'x' {
		uval, err := strconv.ParseUint(str[1:], 16, 64)
		if err == nil {
			return rune(uval), nil
		}
	}
	return 0, fmt.Errorf("unknown character '%s'", str)
}

func characterName(r rune) string {
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

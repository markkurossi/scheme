//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"io"
	"strings"
	"testing"
)

var lexerTests = []struct {
	i string
	o *Token
}{
	{
		i: "(",
		o: &Token{
			Type: '(',
		},
	},
	{
		i: ")",
		o: &Token{
			Type: ')',
		},
	},
	{
		i: "'",
		o: &Token{
			Type: '\'',
		},
	},
	{
		i: "`",
		o: &Token{
			Type: '`',
		},
	},
	{
		i: ",",
		o: &Token{
			Type: ',',
		},
	},
	{
		i: ".",
		o: &Token{
			Type: '.',
		},
	},
	{
		i: "#(",
		o: &Token{
			Type: THashLPar,
		},
	},
	{
		i: "#t",
		o: &Token{
			Type: TBoolean,
			Bool: true,
		},
	},
	{
		i: "#f",
		o: &Token{
			Type: TBoolean,
		},
	},
	{
		i: "#b101",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(2, 5),
		},
	},
	{
		i: "#o077",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(8, 63),
		},
	},
	{
		i: "#d42",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(10, 42),
		},
	},
	{
		i: "#xdeadbeef",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(16, 0xdeadbeef),
		},
	},
	{
		i: "#e42",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(0, 42),
		},
	},
	{
		i: "#e#x42",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(0, 66),
		},
	},
	{
		i: "#e#x-42",
		o: &Token{
			Type:   TNumber,
			Number: NewNumber(0, -66),
		},
	},
	{
		i: `#\alarm`,
		o: &Token{
			Type: TCharacter,
			Char: rune(0x0007),
		},
	},
	{
		i: `#\backspace`,
		o: &Token{
			Type: TCharacter,
			Char: rune(0x0008),
		},
	},
	{
		i: `#\x42`,
		o: &Token{
			Type: TCharacter,
			Char: rune(0x42),
		},
	},
	{
		i: `#\077`,
		o: &Token{
			Type: TCharacter,
			Char: rune(077),
		},
	},
	{
		i: `#\a`,
		o: &Token{
			Type: TCharacter,
			Char: rune('a'),
		},
	},
	{
		i: `"foo"`,
		o: &Token{
			Type: TString,
			Str:  "foo",
		},
	},
	{
		i: `"foo\nbar"`,
		o: &Token{
			Type: TString,
			Str:  "foo\nbar",
		},
	},
	{
		i: "!foo",
		o: &Token{
			Type:       TIdentifier,
			Identifier: "!foo",
		},
	},
	{
		i: "$foo",
		o: &Token{
			Type:       TIdentifier,
			Identifier: "$foo",
		},
	},
	{
		i: "name",
		o: &Token{
			Type:       TIdentifier,
			Identifier: "name",
		},
	},
	{
		i: "+",
		o: &Token{
			Type:       TIdentifier,
			Identifier: "+",
		},
	},
	{
		i: "else",
		o: &Token{
			Type:    TKeyword,
			Keyword: KwElse,
		},
	},
	{
		i: "=>",
		o: &Token{
			Type:    TKeyword,
			Keyword: KwImplies,
		},
	},
}

func TestLexer(t *testing.T) {
	for _, test := range lexerTests {
		lexer := NewLexer("{data}", strings.NewReader(test.i))
		var count int
		for {
			token, err := lexer.Get()
			if err != nil {
				if err == io.EOF {
					if count != 1 {
						t.Errorf("Lexer.Get: should have returned 1 token")
					}
					break
				}
				t.Fatalf("Lexer.Get: %v", err)
			}
			count++
			if !token.Equal(test.o) {
				t.Errorf("unexpected token %v, expected %v", token, test.o)
			}
		}
	}
}

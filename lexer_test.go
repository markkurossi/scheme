//
// Copyright (c) 2022-2025 Markku Rossi
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
			Number: MakeNumber(5),
		},
	},
	{
		i: "#o077",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(63),
		},
	},
	{
		i: "#d42",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(42),
		},
	},
	{
		i: "#xdeadbeef",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(0xdeadbeef),
		},
	},
	{
		i: "#e42",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(42),
		},
	},
	{
		i: "#e#x42",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(66),
		},
	},
	{
		i: "#e#x-42",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(-66),
		},
	},
	{
		i: "#e#x+42",
		o: &Token{
			Type:   TNumber,
			Number: MakeNumber(66),
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
			Type:       TIdentifier,
			Identifier: "else",
		},
	},
	{
		i: "=>",
		o: &Token{
			Type:       TIdentifier,
			Identifier: "=>",
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

func FuzzLexer(f *testing.F) {
	for _, test := range lexerTests {
		f.Add(test.i)
	}
	f.Fuzz(func(t *testing.T, input string) {
		lexer := NewLexer("{data}", strings.NewReader(input))
		for {
			token, err := lexer.Get()
			if err != nil {
				if err == io.EOF {
					break
				}
			}
			_ = token
		}
	})
}

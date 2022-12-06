//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
)

// Parser implements S-expression parser.
type Parser struct {
	lexer *Lexer
}

// NewParser creates a new parser for the input file.
func NewParser(source string, in io.Reader) *Parser {
	return &Parser{
		lexer: NewLexer(source, in),
	}
}

// Next parses the next value.
func (p *Parser) Next() (Value, error) {
	t, err := p.lexer.Get()
	if err != nil {
		return nil, err
	}
	switch t.Type {
	case '\'':
		v, err := p.Next()
		if err != nil {
			return nil, err
		}

		var next Value
		locator, ok := v.(Locator)
		if ok {
			next = NewLocationPair(locator.Location(), v, nil)
		} else {
			next = NewPair(v, nil)
		}

		return NewLocationPair(t.From, KwQuote, next), nil

	case '(':
		var list, cursor Pair
		for {
			t, err = p.lexer.Get()
			if err != nil {
				return nil, err
			}
			if t.Type == ')' {
				return list, nil
			}
			p.lexer.Unget(t)

			v, err := p.Next()
			if err != nil {
				return nil, err
			}

			if cursor == nil {
				cursor = NewLocationPair(t.From, v, nil)
				list = cursor
			} else {
				cdr := NewLocationPair(t.From, v, nil)
				cursor.SetCdr(cdr)
				cursor = cdr
			}
		}

	case THashLPar:
		var elements []Value
		for {
			t, err = p.lexer.Get()
			if err != nil {
				return nil, err
			}
			if t.Type == ')' {
				return Vector(elements), nil
			}
			p.lexer.Unget(t)

			v, err := p.Next()
			if err != nil {
				return nil, err
			}
			elements = append(elements, v)
		}

	case TIdentifier:
		return &Identifier{
			Name: t.Identifier,
		}, nil

	case TBoolean:
		return Boolean(t.Bool), nil

	case TNumber:
		return t.Number, nil

	case TCharacter:
		return Character(t.Char), nil

	case TString:
		return String([]byte(t.Str)), nil

	case TKeyword:
		return t.Keyword, nil

	default:
		return nil, fmt.Errorf("%s: unexpected token: %v", t.From, t)
	}
}

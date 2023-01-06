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

// From returns the parser's current location.
func (p *Parser) From() Point {
	return p.lexer.point
}

// To returns the parser's current location.
func (p *Parser) To() Point {
	return p.lexer.point
}

// SetTo does nothing on parser.
func (p *Parser) SetTo(point Point) {
}

// Errorf returns an error with the location information.
func (p *Parser) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	return fmt.Errorf("%s: %s", p.From(), msg)
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
			next = NewLocationPair(locator.From(), locator.To(), v, nil)
		} else {
			next = NewPair(v, nil)
		}

		return NewLocationPair(t.From, t.To, KwQuote, next), nil

	case '(':
		var list, cursor Pair
		for {
			t, err = p.lexer.Get()
			if err != nil {
				return nil, err
			}
			if t.Type == ')' {
				return list, nil
			} else if t.Type == '.' {
				if cursor == nil {
					return nil, t.Errorf("unexpected token: %v", t)
				}
				t, err = p.lexer.Get()
				if err != nil {
					return nil, err
				}
				p.lexer.Unget(t)

				v, err := p.Next()
				if err != nil {
					return nil, err
				}
				cursor.SetCdr(v)
				cursor.SetTo(t.From)

				t, err = p.lexer.Get()
				if err != nil {
					return nil, err
				}
				if t.Type != ')' {
					return nil, t.Errorf("unexpected input %v, expected: )", t)
				}
				return list, nil
			}
			p.lexer.Unget(t)

			v, err := p.Next()
			if err != nil {
				return nil, err
			}

			if cursor == nil {
				cursor = NewLocationPair(t.From, t.To, v, nil)
				list = cursor
			} else {
				cdr := NewLocationPair(t.From, t.To, v, nil)
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

	case TVU8LPar:
		var elements []byte
		for {
			t, err = p.lexer.Get()
			if err != nil {
				return nil, err
			}
			switch t.Type {
			case ')':
				return ByteVector(elements), nil

			case TNumber:
				v := t.Number.Int64()
				if v < 0 || v > 0xff {
					return nil, t.Errorf("invalid bytevector initializer: %v",
						v)
				}
				elements = append(elements, byte(v))

			default:
				return nil, t.Errorf("invalid bytevector initializer: %v", t)
			}
		}

	case TIdentifier:
		return &Identifier{
			Name:  t.Identifier,
			Point: t.From,
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
		return nil, t.Errorf("unexpected token: %v", t)
	}
}

//
// Copyright (c) 2022-, 20252025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"io"
)

// SexprParser implements S-expression parser.
type SexprParser struct {
	lexer *Lexer
}

// NewSexprParser creates a new parser for the input file.
func NewSexprParser(source string, in io.Reader) *SexprParser {
	return &SexprParser{
		lexer: NewLexer(source, in),
	}
}

// From returns the parser's current location.
func (p *SexprParser) From() Point {
	return p.lexer.point
}

// To returns the parser's current location.
func (p *SexprParser) To() Point {
	return p.lexer.point
}

// SetTo does nothing on parser.
func (p *SexprParser) SetTo(point Point) {
}

// Errorf implements Locator.Errorf.
func (p *SexprParser) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	return fmt.Errorf("%s: %s", p.From(), msg)
}

// Warningf implements Locator.Warningf.
func (p *SexprParser) Warningf(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	fmt.Printf("%s: warning: %s", p.From(), msg)
}

// Infof implements Locator.Infof.
func (p *SexprParser) Infof(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	fmt.Printf("%s: %s", p.From(), msg)
}

// Next parses the next value.
func (p *SexprParser) Next() (Value, error) {
	t, err := p.lexer.Get()
	if err != nil {
		return nil, err
	}
	switch t.Type {
	case '\'':
		v, err := p.Next()
		if err != nil {
			if errors.Is(err, io.EOF) {
				return nil, p.lexer.errf("unexpected EOF")
			}
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
				if errors.Is(err, io.EOF) {
					return nil, p.lexer.errf("unexpected EOF")
				}
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
					if errors.Is(err, io.EOF) {
						return nil, p.lexer.errf("unexpected EOF")
					}
					return nil, err
				}
				p.lexer.Unget(t)

				v, err := p.Next()
				if err != nil {
					if errors.Is(err, io.EOF) {
						return nil, p.lexer.errf("unexpected EOF")
					}
					return nil, err
				}
				cursor.SetCdr(v)
				cursor.SetTo(t.From)

				t, err = p.lexer.Get()
				if err != nil {
					if errors.Is(err, io.EOF) {
						return nil, p.lexer.errf("unexpected EOF")
					}
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
				if errors.Is(err, io.EOF) {
					return nil, p.lexer.errf("unexpected EOF")
				}
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
				if errors.Is(err, io.EOF) {
					return nil, p.lexer.errf("unexpected EOF")
				}
				return nil, err
			}
			if t.Type == ')' {
				return Vector(elements), nil
			}
			p.lexer.Unget(t)

			v, err := p.Next()
			if err != nil {
				if errors.Is(err, io.EOF) {
					return nil, p.lexer.errf("unexpected EOF")
				}
				return nil, err
			}
			elements = append(elements, v)
		}

	case TVU8LPar:
		var elements []byte
		for {
			t, err = p.lexer.Get()
			if err != nil {
				if errors.Is(err, io.EOF) {
					return nil, p.lexer.errf("unexpected EOF")
				}
				return nil, err
			}
			switch t.Type {
			case ')':
				return Bytevector(elements), nil

			case TNumber:
				v, err := Int64(t.Number)
				if err != nil || v < 0 || v > 0xff {
					return nil, t.Errorf("invalid bytevector initializer: '%v'",
						v)
				}
				elements = append(elements, byte(v))

			default:
				return nil, t.Errorf("invalid bytevector initializer: '%v'", t)
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

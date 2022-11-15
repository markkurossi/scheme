//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"unicode"
)

// TokenType specifies input token types.
type TokenType int

// Input tokens.
const (
	TIdentifier TokenType = 256 + iota
	TBoolean
	TNumber
	TCharacter
	TString
	THashLPar
	TCommaAt
)

var tokenTypes = map[TokenType]string{
	TIdentifier: "identifier",
	TBoolean:    "boolean",
	TNumber:     "number",
	TCharacter:  "character",
	TString:     "string",
	THashLPar:   "#(",
	TCommaAt:    ",@",
}

func (t TokenType) String() string {
	name, ok := tokenTypes[t]
	if ok {
		return name
	}
	if t < 256 {
		return fmt.Sprintf("%c", t)
	}
	return fmt.Sprintf("{TokenType %d}", t)
}

// Point defines a position in the input data.
type Point struct {
	Source string
	Line   int // 1-based
	Col    int // 0-based
}

func (p Point) String() string {
	return fmt.Sprintf("%s:%d:%d", p.Source, p.Line, p.Col)
}

// Undefined tests if the point is undefined.
func (p Point) Undefined() bool {
	return p.Line == 0
}

// Token specifies an input token.
type Token struct {
	Type       TokenType
	From       Point
	To         Point
	Identifier string
	Bool       bool
	Number     Number
	Str        string
}

func (t *Token) String() string {
	switch t.Type {
	case TIdentifier:
		return t.Identifier

	case TBoolean:
		var ch rune
		if t.Bool {
			ch = 't'
		} else {
			ch = 'f'
		}
		return fmt.Sprintf("#%c", ch)

	case TNumber:
		return fmt.Sprintf("%v", t.Number)

	case TString:
		var str strings.Builder
		str.WriteRune('"')
		for _, r := range t.Str {
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

	default:
		if t.Type <= 0xff {
			return fmt.Sprintf("%c", t.Type)
		}
		return t.Type.String()
	}
}

// Lexer implement lexical analyser.
type Lexer struct {
	in          *bufio.Reader
	point       Point
	tokenStart  Point
	ungot       *Token
	unread      bool
	unreadRune  rune
	unreadSize  int
	unreadPoint Point
	history     map[int][]rune
}

// NewLexer creates a new lexer for the input.
func NewLexer(source string, in io.Reader) *Lexer {
	return &Lexer{
		in: bufio.NewReader(in),
		point: Point{
			Source: source,
			Line:   1,
			Col:    0,
		},
		history: make(map[int][]rune),
	}
}

func (l *Lexer) err(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	return fmt.Errorf("%s: %s", l.point, msg)
}

// ReadRune reads the next input rune.
func (l *Lexer) ReadRune() (rune, int, error) {
	if l.unread {
		l.point, l.unreadPoint = l.unreadPoint, l.point
		l.unread = false
		return l.unreadRune, l.unreadSize, nil
	}
	r, size, err := l.in.ReadRune()
	if err != nil {
		return r, size, err
	}

	l.unreadRune = r
	l.unreadSize = size
	l.unreadPoint = l.point
	if r == '\n' {
		l.point.Line++
		l.point.Col = 0
	} else {
		l.point.Col++
		l.history[l.point.Line] = append(l.history[l.point.Line], r)
	}

	return r, size, nil
}

// UnreadRune unreads the last rune.
func (l *Lexer) UnreadRune() error {
	l.point, l.unreadPoint = l.unreadPoint, l.point
	l.unread = true
	return nil
}

// FlushEOL discards all remaining input from the current source code
// line.
func (l *Lexer) FlushEOL() error {
	for {
		r, _, err := l.ReadRune()
		if err != nil {
			if err != io.EOF {
				return err
			}
			return nil
		}
		if r == '\n' {
			return nil
		}
	}
}

// Get gest the next token.
func (l *Lexer) Get() (*Token, error) {
	if l.ungot != nil {
		token := l.ungot
		l.ungot = nil
		return token, nil
	}

	for {
		l.tokenStart = l.point
		r, _, err := l.ReadRune()
		if err != nil {
			return nil, err
		}
		if unicode.IsSpace(r) {
			continue
		}
		switch r {
		case ';':
			err = l.FlushEOL()
			if err != nil {
				return nil, err
			}

		case '(', ')', '\'', '`', ',', '.':
			return l.Token(TokenType(r)), nil

		case '#':
			r, _, err := l.ReadRune()
			if err != nil {
				return nil, err
			}
			switch r {
			case '(':
				return l.Token(THashLPar), nil

			case 't', 'f':
				token := l.Token(TBoolean)
				token.Bool = r == 't'
				return token, nil

				// i, e

			case 'b':
				uval, err := l.parseDigit(2)
				if err != nil {
					return nil, err
				}
				token := l.Token(TNumber)
				token.Number = NewNumber(2, uval)
				return token, nil

			case 'o':
				uval, err := l.parseDigit(8)
				if err != nil {
					return nil, err
				}
				token := l.Token(TNumber)
				token.Number = NewNumber(8, uval)
				return token, nil

			case 'd':
				uval, err := l.parseDigit(10)
				if err != nil {
					return nil, err
				}
				token := l.Token(TNumber)
				token.Number = NewNumber(10, uval)
				return token, nil

			case 'x':
				uval, err := l.parseDigit(16)
				if err != nil {
					return nil, err
				}
				token := l.Token(TNumber)
				token.Number = NewNumber(16, uval)
				return token, nil

			default:
				l.UnreadRune()
				return l.Token('#'), nil
			}

		case '"':
			var str strings.Builder
			for {
				r, _, err := l.ReadRune()
				if err != nil {
					return nil, err
				}
				if r == '"' {
					break
				} else if r == '\\' {
					r, _, err = l.ReadRune()
					if err != nil {
						return nil, err
					}
					switch r {
					case '\\', '"', '|', '(':
					case 'a':
						r = '\a'
					case 'f':
						r = '\f'
					case 'n':
						r = '\n'
					case 'r':
						r = '\r'
					case 't':
						r = '\t'
					case 'v':
						r = '\v'
					case 'b':
						r = '\b'
					case '0':
						r = 0

					default:
						return nil, l.err("invalid escape: \\%c", r)
					}
				}
				str.WriteRune(r)
			}
			token := l.Token(TString)
			token.Str = str.String()
			return token, nil

		default:
			if IsIdentifierInitial(r) {
				id := []rune{r}
				for {
					r, _, err := l.ReadRune()
					if err != nil {
						if err == io.EOF {
							break
						}
						return nil, err
					}
					if !IsIdentifierSubsequent(r) {
						l.UnreadRune()
						break
					}
					id = append(id, r)
				}
				token := l.Token(TIdentifier)
				token.Identifier = string(id)
				return token, nil
			}
			if IsDigit10(r) {
				l.UnreadRune()
				uval, err := l.parseDigit(10)
				if err != nil {
					return nil, err
				}

				token := l.Token(TNumber)
				token.Number = NewNumber(0, uval)
				return token, nil
			}
		}
	}
}

func (l *Lexer) parseDigit(base uint64) (uint64, error) {
	var result uint64
	var count int

	for {
		r, _, err := l.ReadRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return 0, err
		}
		var done bool
		var v uint64

		switch base {
		case 2:
			if '0' <= r && r <= '1' {
				v = uint64(r - '0')
			} else {
				done = true
			}

		case 8:
			if '0' <= r && r <= '7' {
				v = uint64(r - '0')
			} else {
				done = true
			}

		case 10:
			if '0' <= r && r <= '9' {
				v = uint64(r - '0')
			} else {
				done = true
			}

		case 16:
			if '0' <= r && r <= '9' {
				v = uint64(r - '0')
			} else if 'a' <= r && r <= 'f' {
				v = uint64(10 + r - 'a')
			} else {
				done = true
			}

		default:
			return 0, l.err("invalid base %v", base)
		}
		if done {
			l.UnreadRune()
			break
		}
		result = result*base + v
		count++
	}
	if count == 0 {
		return 0, l.err("unexpected EOF")
	}

	return result, nil
}

// IsIdentifierInitial tests if the argument rune is a Scheme
// identifier initial character.
func IsIdentifierInitial(r rune) bool {
	switch r {
	// Special initial.
	case '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^':
		return true

	default:
		// Letter
		return unicode.IsLetter(r)
	}
}

// IsIdentifierSubsequent tests if the argument rune is a Scheme
// identifier subsequent character.
func IsIdentifierSubsequent(r rune) bool {
	switch r {
	// Special subsequent.
	case '.', '+', '-':
		return true

	default:
		// Digit or Identifier initial.
		return IsDigit10(r) || IsIdentifierInitial(r)
	}
}

// IsDigit10 tests if the rune is a 10-base digit.
func IsDigit10(r rune) bool {
	return '0' <= r && r <= '9'
}

// Unget pushes the token back to the lexer input stream. The next
// call to Get will return it.
func (l *Lexer) Unget(t *Token) {
	l.ungot = t
}

// Token returns a new token for the argument token type.
func (l *Lexer) Token(t TokenType) *Token {
	return &Token{
		Type: t,
		From: l.tokenStart,
		To:   l.point,
	}
}

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
	Type TokenType
	From Point
	To   Point
	Bool bool
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
	}
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

				// i, e, b, o, d, x

			default:
				l.UnreadRune()
				return l.Token('#'), nil
			}
		}
	}
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

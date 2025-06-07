//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"bufio"
	"fmt"
	"io"
	"math/big"
	"strings"
	"unicode"

	"github.com/markkurossi/scheme/types"
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
	TKeyword
	THashLPar
	TVU8LPar
	TCommaAt
)

var tokenTypes = map[TokenType]string{
	TIdentifier: "identifier",
	TBoolean:    "boolean",
	TNumber:     "number",
	TCharacter:  "character",
	TString:     "string",
	TKeyword:    "keyword",
	THashLPar:   "#(",
	TVU8LPar:    "#vu8(",
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

// Keyword defines a Scheme keyword.
type Keyword int

// Scheme returns the value as a Scheme string.
func (kw Keyword) Scheme() string {
	return kw.String()
}

// Eq tests if the argument value is eq? to this value.
func (kw Keyword) Eq(o Value) bool {
	return kw.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (kw Keyword) Equal(o Value) bool {
	ov, ok := o.(Keyword)
	return ok && kw == ov
}

// Type implements the Value.Type().
func (kw Keyword) Type() *types.Type {
	return types.Unspecified
}

// Unbox implements Value.Unbox.
func (kw Keyword) Unbox() (Value, *types.Type) {
	return kw, kw.Type()
}

func (kw Keyword) String() string {
	name, ok := keywords[kw]
	if ok {
		return name
	}
	return fmt.Sprintf("{Keyword %d}", kw)
}

// Scheme keywords.
const (
	KwElse Keyword = iota
	KwImplies
	KwDefine
	KwDefineConstant
	KwUnquote
	KwUnquoteSplicing
	KwQuote
	KwLambda
	KwIf
	KwSet
	KwBegin
	KwCond
	KwAnd
	KwOr
	KwCase
	KwLet
	KwLetStar
	KwLetrec
	KwDo
	KwDelay
	KwQuasiquote
	KwSchemeApply
	KwPragma
)

var keywords = map[Keyword]string{
	KwElse:            "else",
	KwImplies:         "=>",
	KwDefine:          "define",
	KwDefineConstant:  "define-constant",
	KwUnquote:         "unquote",
	KwUnquoteSplicing: "unquote-splicing",
	KwQuote:           "quote",
	KwLambda:          "lambda",
	KwIf:              "if",
	KwSet:             "set!",
	KwBegin:           "begin",
	KwCond:            "cond",
	KwAnd:             "and",
	KwOr:              "or",
	KwCase:            "case",
	KwLet:             "let",
	KwLetStar:         "let*",
	KwLetrec:          "letrec",
	KwDo:              "do",
	KwDelay:           "delay",
	KwQuasiquote:      "quasiquote",
	KwSchemeApply:     "scheme::apply",
	KwPragma:          "pragma",
}

var keywordNames map[string]Keyword

func init() {
	keywordNames = make(map[string]Keyword)
	for k, v := range keywords {
		keywordNames[v] = k
	}
}

// Point defines a position in the input data.
type Point struct {
	Source string
	Line   int // 1-based
	Col    int // 1-based
}

// From returns the point.
func (p Point) From() Point {
	return p
}

// To returns the point.
func (p Point) To() Point {
	return p
}

// SetTo does nothing on point.
func (p Point) SetTo(point Point) {
}

func isTagged(msg string) bool {
	return []rune(msg)[0] > 0xff
}

// Errorf implements Locator.Errorf.
func (p Point) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		return fmt.Errorf("%s: %s", p, msg)
	}
	return fmt.Errorf("%s: \u2260 %s", p, msg)
}

// Warningf implements Locator.Warningf.
func (p Point) Warningf(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("%s: warning: %s", p, msg)
	} else {
		fmt.Printf("%s: \u2260 warning: %s", p, msg)
	}
}

// Infof implements Locator.Info.
func (p Point) Infof(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("%s: %s", p, msg)
	} else {
		fmt.Printf("%s: \u2260 %s", p, msg)
	}
}

// Locator interface a source location.
type Locator interface {
	From() Point
	To() Point
	SetTo(p Point)
	// Errorf returns an error with the location information.
	Errorf(format string, a ...interface{}) error
	// Warningf prints a warning with the location information.
	Warningf(format string, a ...interface{})
	// Infof prints information with the location information.
	Infof(format string, a ...interface{})
}

var (
	_ Locator = Point{}
	_ Locator = &PlainPair{}
	_ Locator = &LocationPair{}
	_ Locator = &SexprParser{}
)

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
	Number     Value
	NumberType *types.Type
	Char       rune
	Str        string
	Keyword    Keyword
}

// Errorf returns an error with the token location information.
func (t *Token) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	return fmt.Errorf("%s: %s", t.From, msg)
}

// Equal tests if the argument token is equal to this token.
func (t *Token) Equal(o *Token) bool {
	if t.Type != o.Type {
		return false
	}
	switch t.Type {
	case TIdentifier:
		return t.Identifier == o.Identifier

	case TBoolean:
		return t.Bool == o.Bool

	case TNumber:
		return t.Number.Equal(o.Number)

	case TCharacter:
		return t.Char == o.Char

	case TString:
		return t.Str == o.Str

	case TKeyword:
		return t.Keyword == o.Keyword

	default:
		return true
	}
}

func (t *Token) String() string {
	switch t.Type {
	case TIdentifier:
		return t.Identifier

	case TBoolean:
		return BooleanToScheme(t.Bool)

	case TNumber:
		return fmt.Sprintf("%v", t.Number)

	case TCharacter:
		return CharacterToScheme(t.Char)

	case TString:
		return StringToScheme(t.Str)

	case TKeyword:
		return t.Keyword.String()

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
			Col:    1,
		},
		history: make(map[int][]rune),
	}
}

func (l *Lexer) errf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	return fmt.Errorf("%s: \u2260 %s", l.point, msg)
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
		l.point.Col = 1
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

		case '(', ')', '\'', '`', ',':
			return l.Token(TokenType(r)), nil

		case '.':
			r, _, err := l.ReadRune()
			if err != nil {
				if err != io.EOF {
					return nil, err
				}
				return l.Token(TokenType('.')), nil
			}
			if r != '.' {
				l.UnreadRune()
				return l.Token(TokenType('.')), nil
			}
			r, _, err = l.ReadRune()
			if err != nil {
				return nil, err
			}
			if r != '.' {
				l.UnreadRune()
				return nil, l.errf("unexpected character: %c", r)
			}
			token := l.Token(TIdentifier)
			token.Identifier = "..."
			return token, nil

		case '#':
			r, _, err := l.ReadRune()
			if err != nil {
				return nil, err
			}
			switch r {
			case '(':
				return l.Token(THashLPar), nil

			case 'v':
				var vname []rune
				for {
					r, _, err = l.ReadRune()
					if err != nil {
						return nil, err
					}
					if r == '(' {
						break
					}
					vname = append(vname, r)
				}
				name := string(vname)
				switch name {
				case "u8":
					return l.Token(TVU8LPar), nil

				default:
					return nil, l.errf("unknown vector type: '%s'", name)
				}

			case 't', 'f':
				token := l.Token(TBoolean)
				token.Bool = r == 't'
				return token, nil

			case '\\':
				var name []rune
				for {
					r, _, err = l.ReadRune()
					if err != nil {
						if err == io.EOF {
							break
						}
						return nil, err
					}
					if unicode.IsLetter(r) || unicode.IsDigit(r) {
						name = append(name, r)
					} else {
						if len(name) == 0 {
							name = append(name, r)
						} else {
							l.UnreadRune()
						}
						break
					}
				}
				ch, err := lookupCharacter(name)
				if err != nil {
					return nil, err
				}
				token := l.Token(TCharacter)
				token.Char = ch
				return token, nil

			case 'i', 'e', 'b', 'o', 'd', 'x':
				l.UnreadRune()
				return l.parseNumber()

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

					case 'x':
						var code, digit uint64
						for {
							r, _, err = l.ReadRune()
							if err != nil {
								return nil, err
							}
							if r == ';' {
								break
							}
							if '0' <= r && r <= '9' {
								digit = uint64(r - '0')
							} else if 'a' <= r && r <= 'f' {
								digit = uint64(10 + r - 'a')
							} else if 'A' <= r && r <= 'F' {
								digit = uint64(10 + r - 'A')
							} else {
								l.UnreadRune()
								return nil, l.errf("invalid hex literal %c%s",
									r, ": did you forgot terminator ';'?")
							}
							code *= 16
							if code > 0xffffffff {
								l.UnreadRune()
								return nil, l.errf("invalid hex literal")
							}
							code += digit
						}
						r = rune(code)

					default:
						return nil, l.errf("invalid escape: \\%c", r)
					}
				}
				str.WriteRune(r)
			}
			token := l.Token(TString)
			token.Str = str.String()
			return token, nil

		case '+', '-':
			n, _, err := l.ReadRune()
			if err != nil {
				if err != io.EOF {
					return nil, err
				}
			} else if isDigit10(n) {
				l.UnreadRune()
				ival, fval, err := l.parseDigit(10)
				if err != nil {
					return nil, err
				}
				return l.newNumber(false, false, r == '-', ival, fval)
			} else {
				l.UnreadRune()
			}
			token := l.Token(TIdentifier)
			token.Identifier = string(r)
			return token, nil

		default:
			if isIdentifierInitial(r) {
				id := []rune{r}
				for {
					r, _, err := l.ReadRune()
					if err != nil {
						if err == io.EOF {
							break
						}
						return nil, err
					}
					if !isIdentifierSubsequent(r) {
						l.UnreadRune()
						break
					}
					id = append(id, r)
				}
				idName := string(id)
				kw, ok := keywordNames[idName]
				if ok {
					token := l.Token(TKeyword)
					token.Keyword = kw
					return token, nil
				}

				token := l.Token(TIdentifier)
				token.Identifier = string(id)
				return token, nil
			}
			if isDigit10(r) {
				l.UnreadRune()
				ival, fval, err := l.parseDigit(10)
				if err != nil {
					return nil, err
				}
				return l.newNumber(false, false, false, ival, fval)
			}
			return nil, l.errf("unexpected character: %c", r)
		}
	}
}

func (l *Lexer) newNumber(exact, inexact, negative bool, ival *big.Int,
	fval *big.Float) (*Token, error) {

	if exact && inexact {
		return nil, l.errf("invalid number: #e and #i")
	}

	token := l.Token(TNumber)

	if ival != nil {
		if negative {
			zero := big.NewInt(0)
			ival.Sub(zero, ival)
		}
		if exact {
			token.Number = MakeNumber(ival)
			token.NumberType = types.ExactInteger
		} else {
			token.Number = MakeNumber(ival.Int64())
			if inexact {
				token.NumberType = types.InexactInteger
			} else {
				token.NumberType = types.Number
			}
		}
	} else {
		if negative {
			zero := big.NewFloat(0.0)
			fval.Sub(zero, fval)
		}
		if exact {
			token.Number = MakeNumber(fval)
			token.NumberType = types.ExactFloat
		} else {
			f64, accuracy := fval.Float64()
			if accuracy == big.Exact {
				token.Number = MakeNumber(f64)
			} else {
				token.Number = MakeNumber(fval)
			}
			if inexact {
				token.NumberType = types.InexactFloat
			} else {
				token.NumberType = types.Number
			}
		}
	}
	return token, nil
}

func (l *Lexer) parseNumber() (*Token, error) {
	var exact, inexact, negative, hasSign bool
	base := int64(10)

	for {
		// Here we have seen '#' before r.

		r, _, err := l.ReadRune()
		if err != nil {
			return nil, err
		}
		switch r {
		case 'i':
			inexact = true

		case 'e':
			exact = true

		case 'b':
			base = 2

		case 'o':
			base = 8

		case 'd':
			base = 10

		case 'x':
			base = 16

		default:
			l.UnreadRune()
			return nil, l.errf("number: unexpected character: %c", r)
		}

		r, _, err = l.ReadRune()
		if err != nil {
			return nil, err
		}
		if r == '-' || r == '+' {
			negative = r == '-'
			hasSign = true

			r, _, err = l.ReadRune()
			if err != nil {
				return nil, err
			}
		}
		if r == '#' {
			if hasSign {
				l.UnreadRune()
				return nil, l.errf("number: unexpected character: %c", r)
			}
			// Continue from the top.

		} else if isDigit10(r) ||
			'a' <= r && r <= 'f' ||
			'A' <= r && r <= 'F' {
			l.UnreadRune()
			ival, fval, err := l.parseDigit(base)
			if err != nil {
				return nil, err
			}
			return l.newNumber(exact, inexact, negative, ival, fval)
		} else {
			l.UnreadRune()
			return nil, l.errf("number: unexpected character: %c", r)
		}
	}
}

func (l *Lexer) parseDigit(base int64) (*big.Int, *big.Float, error) {
	result := &big.Int{}
	baseBig := big.NewInt(base)

	var count int

	for {
		r, _, err := l.ReadRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, nil, err
		}
		var done bool
		var v int64

		switch base {
		case 2:
			if '0' <= r && r <= '1' {
				v = int64(r - '0')
			} else {
				done = true
			}

		case 8:
			if '0' <= r && r <= '7' {
				v = int64(r - '0')
			} else {
				done = true
			}

		case 10:
			if '0' <= r && r <= '9' {
				v = int64(r - '0')
			} else {
				done = true
			}

		case 16:
			if '0' <= r && r <= '9' {
				v = int64(r - '0')
			} else if 'a' <= r && r <= 'f' {
				v = int64(10 + r - 'a')
			} else {
				done = true
			}

		default:
			return nil, nil, l.errf("digit: invalid base %v", base)
		}
		if done {
			if base == 10 && r == '.' {
				return l.parseFloat(result)
			}
			l.UnreadRune()
			break
		}
		result.Mul(result, baseBig)
		result.Add(result, big.NewInt(v))
		count++
	}
	if count == 0 {
		return nil, nil, l.errf("digit: unexpected EOF")
	}

	return result, nil, nil
}

func (l *Lexer) parseFloat(i *big.Int) (*big.Int, *big.Float, error) {
	input := fmt.Sprintf("%v.", i)
	for {
		r, _, err := l.ReadRune()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, nil, err
		}
		if r < '0' || r > '9' {
			l.UnreadRune()
			break
		}
		input += string(r)
	}

	result, _, err := big.NewFloat(0.0).Parse(input, 10)
	if err != nil {
		return nil, nil, err
	}

	return nil, result, nil
}

// IsIdentifierInitial tests if the argument rune is a Scheme
// identifier initial character.
func isIdentifierInitial(r rune) bool {
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
func isIdentifierSubsequent(r rune) bool {
	switch r {
	// Special subsequent.
	case '.', '+', '-':
		return true

	default:
		// Digit or Identifier initial.
		return isDigit10(r) || isIdentifierInitial(r)
	}
}

// IsDigit8 tests if the rune is a 8-base digit.
func isDigit8(r rune) bool {
	return '0' <= r && r <= '8'
}

// IsDigit10 tests if the rune is a 10-base digit.
func isDigit10(r rune) bool {
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

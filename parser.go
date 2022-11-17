//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
	"os"
)

// Parser implements S-expression parser.
type Parser struct {
	input io.ReadCloser
	lexer *Lexer
}

// NewParser creates a new parser for the input file.
func NewParser(file string) (*Parser, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	return &Parser{
		input: f,
		lexer: NewLexer(file, f),
	}, nil
}

// Close closes the parser input stream.
func (p *Parser) Close() error {
	return p.input.Close()
}

// Next parses the next value.
func (p *Parser) Next() (Value, error) {
	t, err := p.lexer.Get()
	if err != nil {
		return nil, err
	}
	switch t.Type {
	case '(':
		var list, cursor *Cons
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
				cursor = &Cons{
					Car: v,
				}
				list = cursor
			} else {
				cdr := &Cons{
					Car: v,
				}
				cursor.Cdr = cdr
				cursor = cdr
			}
		}

	case TIdentifier:
		return &Identifier{
			Name: t.Identifier,
		}, nil

	case TBoolean:
		return &Boolean{
			Bool: t.Bool,
		}, nil

	case TNumber:
		return t.Number, nil

	case TCharacter:
		return &Character{
			Char: t.Char,
		}, nil

	case TString:
		return &String{
			Str: t.Str,
		}, nil

	default:
		return nil, fmt.Errorf("unexpected token: %v", t)
	}
}

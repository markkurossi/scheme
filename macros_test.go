//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"testing"
)

var macroPatternTests = []struct {
	pattern string
	source  string
}{
	{
		pattern: `
(define-syntax def
  (syntax-rules ()
    ((def f (p ...) body)
     (define (f p ...)
       (body)))))`,
		source: `(def f (x) (+ x 42))`,
	},
	{
		pattern: `
(define-syntax def
  (syntax-rules ()
    ((def _ f (p ...) body)
     (define (f p ...)
       body))))`,
		source: `(def "comment" f (x) (+ x 42))`,
	},
	{
		pattern: `
(define-syntax bind-to-zero
  (syntax-rules ()
    ((bind-to-zero id)
     (define id 0))))`,
		source: `(bind-to-zero x)`,
	},
	{
		pattern: `
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test (begin result1 result2 ...)))))`,
		source: `(when #t (display "true") (newline))`,
	},
	{
		pattern: `
(define-syntax unless
  (syntax-rules()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))`,
		source: `(unless #t (display "true\n"))`,
	},
}

func TestMacroPattern(t *testing.T) {
	for idx, test := range macroPatternTests {
		v, err := parseSexpr(test.pattern)
		if err != nil {
			t.Fatal(err)
		}
		parser := NewParser(nil)

		list, ok := ListPairs(v)
		if !ok {
			t.Fatalf("invalid value: %v", v)
		}

		ast, err := parser.parseMacro(nil, MacroDefine, list)
		if err != nil {
			t.Fatalf("test-%v: %v", idx, err)
		}
		macro := ast.(*ASTMacro)

		fmt.Printf("test-%v: variables:", idx)
		for k := range macro.Variables {
			fmt.Printf(" %v", k)
		}
		fmt.Println()

		v, err = parseSexpr(test.source)
		if err != nil {
			t.Fatal(err)
		}

		_, env, ok := macro.Pattern.Match([]Pair{
			NewPair(v, nil),
		})
		if !ok {
			t.Errorf("test-%v: no match", idx)
		}
		if env != nil {
			fmt.Printf("test-%v: matches:", idx)
			for k, v := range env.bindings {
				fmt.Printf(" %v=%v", k, v)
			}
			fmt.Println()
		}
	}
}

func parseSexpr(input string) (Value, error) {
	return NewSexprParser("{data}", strings.NewReader(input)).Next()
}

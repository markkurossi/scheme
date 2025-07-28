//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"strings"
	"testing"
)

var macroPatternTests = []struct {
	pattern string
	source  string
	result  string
}{
	{
		pattern: `
(define-syntax def
  (syntax-rules ()
    ((def f (p ...) body)
     (define (f p ...)
       body))))`,
		source: `(def f (x) (+ x 42))`,
		result: `(define (f x) (+ x 42))`,
	},
	{
		pattern: `
(define-syntax def
  (syntax-rules ()
    ((def _ f (p ...) body)
     (define (f p ...)
       body))))`,
		source: `(def "comment" f (x) (+ x 42))`,
		result: `(define (f x) (+ x 42))`,
	},
	{
		pattern: `
(define-syntax bind-to-zero
  (syntax-rules ()
    ((bind-to-zero id)
     (define id 0))))`,
		source: `(bind-to-zero x)`,
		result: `(define x 0)`,
	},
	{
		pattern: `
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test (begin result1 result2 ...)))))`,
		source: `(when #t (display "true") (newline))`,
		result: `(if #t (begin (display "true") (newline)))`,
	},
	{
		pattern: `
(define-syntax unless
  (syntax-rules()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))`,
		source: `(unless #t (display "true\n"))`,
		result: `(if (not #t) (begin (display "true\n")))`,
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

		v, err = parseSexpr(test.source)
		if err != nil {
			t.Fatal(err)
		}

		rule, env := macro.Match(v)
		if rule == nil {
			t.Errorf("test-%v: no match", idx)
			continue
		}
		expanded, err := Eval(rule.Template, env)
		if err != nil {
			t.Errorf("test-%v: template expansion failed: %v", idx, err)
			continue
		}

		expected, err := parseSexpr(test.result)
		if err != nil {
			t.Fatal(err)
		}

		if !Equal(expanded, expected) {
			t.Errorf("test-%v: expansion failed: got %v, expected %v",
				idx, expanded, expected)
		}
	}
}

func parseSexpr(input string) (Value, error) {
	return NewSexprParser("{data}", strings.NewReader(input)).Next()
}

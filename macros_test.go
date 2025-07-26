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
		pattern: `(def f (p ...) body)`,
		source:  `(def f (x) (+ x 42))`,
	},
	{
		pattern: `(def _ f (p ...) body)`,
		source:  `(def "comment" f (x) (+ x 42))`,
	},
	{
		pattern: `(bind-to-zero id)`,
		source:  `(bind-to-zero x)`,
	},
	{
		pattern: `(when test result1 result2 ...)`,
		source:  `(when #t (display "true") (newline))`,
	},
	{
		pattern: `(unless test result1 result2 ...)`,
		source:  `(unless #t (display "true\n"))`,
	},
}

func TestMacroPattern(t *testing.T) {
	for idx, test := range macroPatternTests {
		v, err := parseSexpr(test.pattern)
		if err != nil {
			t.Fatal(err)
		}
		parser := NewParser(nil)
		macro := &ASTMacro{
			Literals:  make(map[string]*Symbol),
			Variables: make(map[string]*Symbol),
		}
		pair, ok := v.(Pair)
		if !ok {
			t.Fatalf("expected pair: %v", v)
		}
		sym, ok := pair.Car().(*Symbol)
		if !ok {
			t.Fatalf("expected symbol: %v", pair.Car())
		}
		macro.Literals[sym.Name] = sym

		srpattern, err := parser.parseSyntaxRule(macro, v)
		if err != nil {
			t.Fatal(err)
		}
		fmt.Printf("%v => %v\n", test.pattern, srpattern)
		fmt.Printf(" - variables:")
		for k := range macro.Variables {
			fmt.Printf(" %v", k)
		}
		fmt.Println()

		v, err = parseSexpr(test.source)
		if err != nil {
			t.Fatal(err)
		}

		_, env, ok := srpattern.Match([]Pair{
			NewPair(v, nil),
		})
		if !ok {
			t.Errorf("test-%v: no match", idx)
		}
		if env != nil {
			fmt.Printf("matches:\n")
			for k, v := range env.bindings {
				fmt.Printf(" - %-10v: %v\n", k, v)
			}
		}
	}
}

func parseSexpr(input string) (Value, error) {
	return NewSexprParser("{data}", strings.NewReader(input)).Next()
}

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
	symbol  string
	pattern string
	source  string
}{
	{
		symbol:  "def",
		pattern: `(def f (p ...) body)`,
		source:  `(def f (x) (+ x 42))`,
	},
	{
		symbol:  "def",
		pattern: `(def _ f (p ...) body)`,
		source:  `(def "comment" f (x) (+ x 42))`,
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
			Literals: make(map[string]bool),
		}
		macro.Literals[test.symbol] = true

		srpattern, err := parser.parseSyntaxRule(macro, v)
		if err != nil {
			t.Fatal(err)
		}
		fmt.Printf("%v => %v\n", test.pattern, srpattern)

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

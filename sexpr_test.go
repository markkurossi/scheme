//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"io"
	"strings"
	"testing"
)

var parserTests = []struct {
	i string
	o Value
}{
	{
		i: "(1 2)",
		o: NewPair(MakeNumber(1), NewPair(MakeNumber(2), nil)),
	},
	{
		i: "foo",
		o: &Identifier{
			Name: "foo",
		},
	},
	{
		i: "#t",
		o: Boolean(true),
	},
	{
		i: "1",
		o: MakeNumber(1),
	},
	{
		i: `"foo"`,
		o: String([]byte("foo")),
	},
	{
		i: "else",
		o: KwElse,
	},
}

func TestParser(t *testing.T) {
	for _, test := range parserTests {
		parser := NewSexprParser("{data}", strings.NewReader(test.i))
		for {
			v, err := parser.Next()
			if err != nil {
				if err == io.EOF {
					break
				}
				t.Fatalf("Parser.Next failed: %v", err)
			}
			if !v.Equal(test.o) {
				t.Logf("unexpected value %v, exptected %v\n", v, test.o)
			}
		}
	}
}

//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"testing"
)

var typecheckTests = []struct {
	name string
	data string
}{
	{
		name: "non-lambda function",
		data: `("foo" 1 2)`,
	},
	{
		name: "native argument count",
		data: `(string-length 1 2)`,
	},
	{
		name: "back-reference argument count",
		data: `
(define (bar a)
  (+ a 1))
(define (foo)
  (bar 1 2))
`,
	},
	{
		name: "forward-reference argument count",
		data: `
(define (foo)
  (bar 1 2))
(define (bar a)
  (+ a 1))
`,
	},
	{
		name: "redefine symbol",
		data: `
(define (foo)
  (+ 1 2))
(define (foo a)
  (+ a 1))
`,
	},
	{
		name: "set invalid value",
		data: `
(define num #e10)
(set! num #t)
`,
	},
	{
		name: "invalid fixed argument type",
		data: `
(string-length 1)
`,
	},
}

func TestTypecheck(t *testing.T) {
	for idx, test := range typecheckTests {
		scm, err := New()
		if err != nil {
			t.Fatal(err)
		}
		scm.Params.Quiet = true

		_, err = scm.Eval(fmt.Sprintf("test-%d", idx),
			strings.NewReader(test.data))
		if err == nil {
			t.Errorf("test-%d: error %s not detected", idx, test.name)
		}
	}
}
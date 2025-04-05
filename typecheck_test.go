//
// Copyright (c) 2022-2025 Markku Rossi
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
	{
		name: "let init argument count",
		data: `
(let ((a (string-length 1 2))
      (b 1))
  (+ a b))
`,
	},
	{
		name: "let body argument count",
		data: `
(let ((f (lambda (a) (+ a 1)))
      (b 1))
  (f 1 2))
`,
	},
	{
		name: "let* back-reference argument count",
		data: `
(let* ((f (lambda (a) (+ a 1)))
       (b (f 1 2)))
  (display b)
  (newline))
`,
	},
	{
		name: "letrec forward-reference argument count",
		data: `
(letrec ((b (lambda (a) (f 1 2)))
         (f (lambda (a) (+ a 1))))
  (display (b 1))
  (newline))
`,
	},
}

func TestTypecheck(t *testing.T) {
	for idx, test := range typecheckTests {
		scm := newTestScheme(t)

		scm.Params.Quiet = true

		_, err := scm.Eval(fmt.Sprintf("test-%d", idx),
			strings.NewReader(test.data))

		if err == nil {
			t.Errorf("test-%d: error %s not detected", idx, test.name)
		}
	}
}

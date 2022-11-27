//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"testing"
)

var vmTests = []struct {
	i string
	v Value
	o string
}{
	{
		i: `(display "The length of \"Hello, world!\" is ")
(display (string-length "Hello, world!"))
(display ".")
(newline)`,
		o: `The length of "Hello, world!" is 13.
`,
	},
	{
		i: `(define (print msg) (display msg) (newline))
(print "Hello, lambda!")
(print "Hello, world!")`,
		o: `Hello, lambda!
Hello, world!
`,
	},
	{
		i: `(define msg "Hello, msg!")
(set! msg "Hello, set!")
(print msg)
`,
		o: `Hello, set!
`,
	},
	{
		i: `
(define (say-maker header msg trailer)
  (lambda (pre post)
    (display header)
    (display pre)
    (display msg)
    (display post)
    (display trailer)
    (newline)))

(define a (say-maker "<html>" "Hello, a!" "</html>"))
(define b (say-maker "<div>" "Hello, b!" "</div>"))

(a "(" ")")
(b "{" "}")
`,
		o: `<html>(Hello, a!)</html>
<div>{Hello, b!}</div>
`,
	},
	{
		i: `(+ 1 2 3)`,
		v: NewNumber(0, 6),
		o: ``,
	},
	{
		i: `(+)`,
		v: NewNumber(0, 0),
		o: ``,
	},
	{
		i: `(* 1 2 3)`,
		v: NewNumber(0, 6),
		o: ``,
	},
	{
		i: `(*)`,
		v: NewNumber(0, 1),
		o: ``,
	},
}

func TestVM(t *testing.T) {
	scm, err := New()
	if err != nil {
		t.Fatalf("failed to create virtual machine: %v", err)
	}
	stdout := &strings.Builder{}
	scm.Stdout = stdout

	for idx, test := range vmTests {
		stdout.Reset()

		v, err := scm.Eval(fmt.Sprintf("test-%d", idx),
			strings.NewReader(test.i))
		if err != nil {
			t.Fatalf("Eval failed: %v", err)
		}
		if !Equal(v, test.v) {
			t.Errorf("Eval failed: got %v, expected %v", v, test.v)
		}
		output := stdout.String()
		if output != test.o {
			t.Errorf("unexpected output: got '%v', expected '%v'",
				output, test.o)
		}
	}
}

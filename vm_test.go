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
		i: `(define (print msg) (display msg) (newline))
(define msg "Hello, msg!")
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
		i: `(begin 1 2 3 4)`,
		v: NewNumber(4),
		o: ``,
	},
	{
		i: `(define v (cons 1 2)) (set-car! v 42) v`,
		v: NewPair(NewNumber(42), NewNumber(2)),
	},
	{
		i: `(define v (cons 1 2)) (set-cdr! v 42) v`,
		v: NewPair(NewNumber(1), NewNumber(42)),
	},
	{
		i: `((lambda x x) 3 4 5 6)`,
		v: NewPair(NewNumber(3),
			NewPair(NewNumber(4),
				NewPair(NewNumber(5),
					NewPair(NewNumber(6),
						nil)))),
	},
	{
		i: `((lambda (x y . z) z) 3 4 5 6)`,
		v: NewPair(NewNumber(5),
			NewPair(NewNumber(6),
				nil)),
	},
	{
		i: `'()`,
		v: nil,
	},
	{
		i: `'(1 2)`,
		v: NewPair(NewNumber(1), NewPair(NewNumber(2), nil)),
	},
	{
		i: `(quote ())`,
		v: nil,
	},
	{
		i: `(quote (1 2))`,
		v: NewPair(NewNumber(1), NewPair(NewNumber(2), nil)),
	},
	{
		i: `'#t`,
		v: Boolean(true),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (* x y))
`,
		v: NewNumber(6),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))
`,
		v: NewNumber(35),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))
`,
		v: NewNumber(70),
	},
	{
		i: `
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88))
`,
		v: Boolean(true),
	},
}

func TestVM(t *testing.T) {
	for idx, test := range vmTests {
		scm, err := New()
		if err != nil {
			t.Fatalf("failed to create virtual machine: %v", err)
		}
		stdout := &strings.Builder{}
		scm.Stdout = NewPort(stdout)

		v, err := scm.Eval(fmt.Sprintf("test-%d", idx),
			strings.NewReader(test.i))
		if err != nil {
			t.Fatalf("Test %d: Eval failed: %v", idx, err)
		}
		if !Equal(v, test.v) {
			fmt.Printf("Test %d:\n%s\n", idx, vmTests[idx].i)
			fmt.Printf(" - result mismatch: got %v, expected %v\n",
				v, test.v)
			t.Errorf("Test %d: Eval failed: got %v, expected %v",
				idx, v, test.v)
		}
		output := stdout.String()
		if output != test.o {
			fmt.Printf("Test %d:\n%s\n", idx, vmTests[idx].i)
			fmt.Printf(" - output mismatch: got '%v', expected '%v'\n",
				output, test.o)
			t.Errorf("unexpected output: got '%v', expected '%v'",
				output, test.o)
		}
	}
}

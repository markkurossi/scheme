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
	{
		i: `(begin 1 2 3 4)`,
		v: NewNumber(0, 4),
		o: ``,
	},
	{
		i: `(if #t 1 2)`,
		v: NewNumber(0, 1),
		o: ``,
	},

	{
		i: `(if #f 1 2)`,
		v: NewNumber(0, 2),
		o: ``,
	},
	{
		i: `(pair? (cons 1 2))`,
		v: Boolean(true),
	},
	{
		i: `(pair? 3)`,
		v: Boolean(false),
	},
	{
		i: `(cons 1 2)`,
		v: NewPair(NewNumber(0, 1), NewNumber(0, 2)),
	},
	{
		i: `(car (cons 1 2))`,
		v: NewNumber(0, 1),
	},
	{
		i: `(cdr (cons 1 2))`,
		v: NewNumber(0, 2),
	},
	{
		i: `(define v (cons 1 2)) (set-car! v 42) v`,
		v: NewPair(NewNumber(0, 42), NewNumber(0, 2)),
	},
	{
		i: `(define v (cons 1 2)) (set-cdr! v 42) v`,
		v: NewPair(NewNumber(0, 1), NewNumber(0, 42)),
	},
	{
		i: `((lambda x x) 3 4 5 6)`,
		v: NewPair(NewNumber(0, 3),
			NewPair(NewNumber(0, 4),
				NewPair(NewNumber(0, 5),
					NewPair(NewNumber(0, 6),
						nil)))),
	},
	{
		i: `((lambda (x y . z) z) 3 4 5 6)`,
		v: NewPair(NewNumber(0, 5),
			NewPair(NewNumber(0, 6),
				nil)),
	},
	{
		i: `(null? (list))`,
		v: Boolean(true),
	},
	{
		i: `(null? (list 1))`,
		v: Boolean(false),
	},
	{
		i: `(list? (list))`,
		v: Boolean(true),
	},
	{
		i: `(list? (list 1))`,
		v: Boolean(true),
	},
	{
		i: `(list? 1)`,
		v: Boolean(false),
	},
	{
		i: `(length (list))`,
		v: NewNumber(0, 0),
	},
	{
		i: `(length (list 1 2 3))`,
		v: NewNumber(0, 3),
	},
	{
		i: `(list-tail (list 1 2 3) 2)`,
		v: NewPair(NewNumber(0, 3), nil),
	},
	{
		i: `(list-tail (list 1 2 3) 3)`,
		v: nil,
	},
	{
		i: `(list-ref (list 1 2 3) 0)`,
		v: NewNumber(0, 1),
	},
	{
		i: `(list-ref (list 1 2 3) 1)`,
		v: NewNumber(0, 2),
	},
	{
		i: `(list-ref (list 1 2 3) 2)`,
		v: NewNumber(0, 3),
	},
	{
		i: `'()`,
		v: nil,
	},
	{
		i: `'(1 2)`,
		v: NewPair(NewNumber(0, 1), NewPair(NewNumber(0, 2), nil)),
	},
	{
		i: `(quote ())`,
		v: nil,
	},
	{
		i: `(quote (1 2))`,
		v: NewPair(NewNumber(0, 1), NewPair(NewNumber(0, 2), nil)),
	},
	{
		i: `'#t`,
		v: Boolean(true),
	},
	{
		i: `(not 3)`,
		v: Boolean(false),
	},
	{
		i: `(not (list 3))`,
		v: Boolean(false),
	},
	{
		i: `(not #f)`,
		v: Boolean(true),
	},
	{
		i: `(not '())`,
		v: Boolean(false),
	},
	{
		i: `(not (list))`,
		v: Boolean(false),
	},
	{
		i: `(not nil)`,
		v: Boolean(false),
	},
	{
		i: `(boolean? #f)`,
		v: Boolean(true),
	},
	{
		i: `(boolean? 0)`,
		v: Boolean(false),
	},
	{
		i: `(boolean? '())`,
		v: Boolean(false),
	},
	{
		i: `(eq? #t #t)`,
		v: Boolean(true),
	},
	{
		i: `(eq? #f #f)`,
		v: Boolean(true),
	},
	{
		i: `(eq? 'a 'a)`,
		v: Boolean(true),
	},
	{
		i: `(eq? 42 42)`,
		v: Boolean(true),
	},
	{
		i: `(eq? #e42 #e42)`,
		v: Boolean(true),
	},
	{
		i: `(eq? #\a #\a)`,
		v: Boolean(true),
	},
	{
		i: `(eq? '() '())`,
		v: Boolean(true),
	},
	// XXX pairs, vectors, strings
	// XXX procedures
	{
		i: `(eq? #t #\a)`,
		v: Boolean(false),
	},
	{
		i: `(eq? #t #f)`,
		v: Boolean(false),
	},
	{
		i: `(eq? 'a 'b)`,
		v: Boolean(false),
	},
	{
		i: `(eq? 42 #e42)`,
		v: Boolean(false),
	},
	{
		i: `(eq? #\a #\b)`,
		v: Boolean(false),
	},
	{
		i: `(eq? '() '(a))`,
		v: Boolean(false),
	},
	{
		i: `(eq? (cons 1 2) (cons 1 2))`,
		v: Boolean(false),
	},
	// XXX pairs
	{
		i: `(vector? #(1 2 3))`,
		v: Boolean(true),
	},
	{
		i: `(vector? 1)`,
		v: Boolean(false),
	},
	{
		i: `(make-vector 3 #t)`,
		v: Vector([]Value{Boolean(true), Boolean(true), Boolean(true)}),
	},
	{
		i: `(vector-length #())`,
		v: NewNumber(0, 0),
	},
	{
		i: `(vector-length #(1 2 3))`,
		v: NewNumber(0, 3),
	},
	// XXX strings
	// XXX procedures
	{
		i: `(equal? 'a 'a)`,
		v: Boolean(true),
	},
	{
		i: `(equal? '(a) '(a))`,
		v: Boolean(true),
	},
	{
		i: `(equal? '(a (b) c) '(a (b) c))`,
		v: Boolean(true),
	},
	{
		i: `(equal? "abc" "abc")`,
		v: Boolean(true),
	},
	{
		i: `(equal? 2 2)`,
		v: Boolean(true),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (* x y))
`,
		v: NewNumber(0, 6),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))
`,
		v: NewNumber(0, 35),
	},
	{
		i: `
(let ((x 2)
      (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))
`,
		v: NewNumber(0, 70),
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
		scm.Stdout = stdout

		v, err := scm.Eval(fmt.Sprintf("test-%d", idx),
			strings.NewReader(test.i))
		if err != nil {
			t.Fatalf("Test %d: Eval failed: %v", idx, err)
		}
		if !Equal(v, test.v) {
			fmt.Printf("Test %d:\n%s\n", idx, vmTests[idx].i)
			t.Errorf("Test %d: Eval failed: got %v, expected %v",
				idx, v, test.v)
		}
		output := stdout.String()
		if output != test.o {
			t.Errorf("unexpected output: got '%v', expected '%v'",
				output, test.o)
		}
	}
}

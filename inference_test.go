//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

// go test -run TestInference

package scheme

import (
	"fmt"
	"strings"
	"testing"

	"github.com/markkurossi/scheme/types"
)

var inferenceTests = []struct {
	d string
	t *types.Type
}{
	{
		d: `#t`,
		t: types.Boolean,
	},
	{
		d: `1`,
		t: types.InexactInteger,
	},
	{
		d: `1.0`,
		t: types.InexactFloat,
	},
	{
		d: `#e1`,
		t: types.ExactInteger,
	},
	{
		d: `#e1.0`,
		t: types.ExactFloat,
	},
	{
		d: `#\t`,
		t: types.Character,
	},
	{
		d: `"Hello, world!"`,
		t: types.String,
	},
	{
		d: `(define limit #i1)`,
		t: types.InexactInteger,
	},
	{
		d: `(define limit #i1) limit`,
		t: types.InexactInteger,
	},
	{
		d: `(define limit 1)`,
		t: types.Number,
	},
	{
		d: `(define limit 1) limit`,
		t: types.Number,
	},

	{
		d: `(let ((x 1)) x)`,
		t: types.InexactInteger,
	},
	// 10
	{
		d: `(define (loop a) (loop (+ a 1))) (loop 0)`,
		t: &types.Type{
			Enum: types.EnumTypeVar,
		},
	},
	{
		d: `(define (odd n) (e (+ n 1))) (define (e n) (odd (+ n 1))) (odd 0)`,
		t: &types.Type{
			Enum: types.EnumTypeVar,
		},
	},
	{
		d: `(define init (+ #e1 1)) (set! init 42)`,
		t: types.ExactInteger,
	},
	{
		d: `(let ((init (+ #e1 1))) (set! init 42) init)`,
		t: types.InexactInteger,
	},
	{
		d: `(begin #t "foo" 3)`,
		t: types.InexactInteger,
	},
	{
		d: `(and #t #f)`,
		t: types.Boolean,
	},
	{
		d: `(and)`,
		t: types.Boolean,
	},
	{
		d: `(or #t #f)`,
		t: types.Boolean,
	},
	{
		d: `(or)`,
		t: types.Boolean,
	},
	{
		d: `(cond ((> 3 2) "greater") ((< 3 2) "less") (else "equal"))`,
		t: types.String,
	},
	// 20
	{
		d: `
(letrec ((obj '#(1 2 3))
         (iter
          (lambda (idx)
            (if (< idx (vector-length obj))
                (vector-ref obj idx)
                (vector-ref obj idx))
            idx)))
  iter)
`,
		t: &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.InexactInteger},
			Return: types.InexactInteger,
		},
	},
	{
		d: `
(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))
`,
		t: &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Number},
			Return: types.Number,
		},
	},
}

func TestInference(t *testing.T) {
	for idx, test := range inferenceTests {
		name := fmt.Sprintf("test-%d", idx)
		scm := newTestScheme(t)

		library, err := scm.Load(name, strings.NewReader(test.d))
		if err != nil {
			t.Fatal(err)
		}
		values, ok := ListValues(library)
		if !ok || len(values) != 5 {
			t.Fatalf("%s: invalid library: %v", name, library)
			continue
		}
		lib, ok := values[4].(*Library)
		if !ok {
			t.Fatalf("%s: invalid library: %v", name, library)
		}
		inferer := NewInferer(scm, lib.Body.Items)
		typ, err := inferer.Infer(lib.Body)
		if err != nil {
			t.Logf("%s: infer error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Fatalf(" \u2570 %s", err)
		}
		if (test.t.Enum == types.EnumTypeVar &&
			typ.Enum != types.EnumTypeVar) ||
			(test.t.Enum != types.EnumTypeVar && !typ.IsA(test.t)) {
			t.Logf("%s: type error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Errorf(" \u2570 expected %v, got %v", test.t, typ)
		}
	}
}

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

	"github.com/markkurossi/scheme/types"
)

var instantiateTests = []struct {
	scheme *InferScheme
	result *types.Type
}{
	{
		scheme: &InferScheme{
			Type: types.InexactInteger,
		},
		result: types.InexactInteger,
	},
	{},
}

func TestInstantiate(t *testing.T) {
	scm := newTestScheme(t)
	inferer := NewInferer(scm, nil)

	scheme := &InferScheme{
		Type: types.InexactInteger,
	}
	got := scheme.Instantiate(inferer)
	if !got.IsA(types.InexactInteger) {
		t.Errorf("expected %v, got %v\n", types.InexactInteger, got)
	}

	tv := inferer.newTypeVar()
	scheme = &InferScheme{
		Variables: []*types.Type{tv},
		Type:      tv,
	}
	got = scheme.Instantiate(inferer)
	if got.IsA(tv) {
		t.Errorf("TypeVar %v not substituted\n", tv)
	} else {
		t.Logf("instantiate: %v => %v\n", scheme, got)
	}
}

func TestApply(t *testing.T) {
	scm := newTestScheme(t)
	inferer := NewInferer(scm, nil)
	env := inferer.NewEnv()

	subst := make(InferSubst)
	scheme := &InferScheme{
		Type: &types.Type{
			Enum: types.EnumLambda,
			Args: []*types.Type{
				inferer.newTypeVar(),
			},
			Rest:   inferer.newTypeVar(),
			Return: inferer.newTypeVar(),
		},
	}
	scheme.Variables = []*types.Type{
		scheme.Type.Args[0],
		scheme.Type.Rest,
		scheme.Type.Return,
	}
	types := []*InferScheme{
		env.Generalize(types.ExactInteger),
		env.Generalize(types.String),
		env.Generalize(types.ExactFloat),
	}
	for i, t := range types {
		subst[scheme.Variables[i].TypeVar] = t
	}

	scheme = subst.Apply(scheme)
	if !scheme.Type.Args[0].IsA(types[0].Type) {
		t.Errorf("Args[0] type mismatch")
	}
	if !scheme.Type.Rest.IsA(types[1].Type) {
		t.Errorf("Rest type mismatch")
	}
	if !scheme.Type.Return.IsA(types[2].Type) {
		t.Errorf("Return type mismatch")
	}
}

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
		d: `(define limit 1)`,
		t: types.InexactInteger,
	},
	{
		d: `(define limit 1) limit`,
		t: types.InexactInteger,
	},

	{
		d: `(let ((x 1)) x)`,
		t: types.InexactInteger,
	},
	{
		d: `(define (loop a) (loop (+ a 1))) (loop 0)`,
		t: types.Any,
	},
	{
		d: `(define (odd n) (e (+ n 1))) (define (e n) (odd (+ n 1))) (odd 0)`,
		t: types.Any,
	},
	{
		d: `(define init (+ #e1 1)) (set! init 42)`,
		t: types.Number,
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

		_, typ, err := lib.Body.Infer(inferer.NewEnv())
		if err != nil {
			t.Logf("%s: infer error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Fatalf(" \u2570 %s", err)
		}
		if !typ.IsA(test.t) {
			t.Logf("%s: type error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Errorf(" \u2570 expected %v, got %v", test.t, typ)
		}
	}
}

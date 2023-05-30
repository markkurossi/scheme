//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package main

import (
	"strings"
	"testing"

	"github.com/markkurossi/scheme"
)

func TestAPI(t *testing.T) {
	scm, err := scheme.New()
	if err != nil {
		t.Fatalf("scheme.New(): %v", err)
	}

	v, err := scm.Eval("{data}", strings.NewReader(`(define msg "foo")`))
	if err != nil {
		t.Fatalf("scheme.Eval: %v", err)
	}
	str, ok := scheme.IsString(v)
	if !ok || str != "foo" {
		t.Errorf("eval: got %v, expected 'foo'", str)
	}

	_, err = scm.Eval("{data}", strings.NewReader(`
(define (add1 num) (+ 1 num))
`))
	if err != nil {
		t.Fatalf("scheme.Eval: %v", err)
	}

	v, err = scm.Global("add1")
	if err != nil {
		t.Fatalf("add1 not defined: %v", err)
	}
	v, err = scm.Apply(v, []scheme.Value{scheme.NewNumber(41)})
	if err != nil {
		t.Fatalf("scm.Apply: %v", err)
	}
	number, err := scheme.Int64(v)
	if err != nil {
		t.Fatalf("expected number: %v", err)
	}
	if number != 42 {
		t.Errorf("expected 42, got %v", number)
	}
}

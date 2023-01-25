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

	v, err := scm.Eval("{data}", strings.NewReader(`(define msg "foo") msg`))
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

	// get value for "add1"
	// apply add1 to 41
	// check result is 42
}

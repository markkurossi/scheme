//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"testing"
)

func TestCompiler(t *testing.T) {
	scm, err := New()
	if err != nil {
		t.Fatalf("failed to create virtual machine: %v", err)
	}
	v, err := scm.EvalFile("testdata/hello.scm")
	if err != nil {
		t.Fatalf("EvalFile failed: %v", err)
	}
	fmt.Printf("=> %v\n", v)
}

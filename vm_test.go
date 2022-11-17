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
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("failed to create virtual machine: %v", err)
	}
	v, err := vm.EvalFile("testdata/hello.scm")
	if err != nil {
		t.Fatalf("EvalFile failed: %v", err)
	}
	fmt.Printf("=> %v\n", v)
}

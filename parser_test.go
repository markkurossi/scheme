//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
	"testing"
)

func TestParser(t *testing.T) {
	p, err := NewParser("testdata/wasm.scm")
	if err != nil {
		t.Fatalf("failed to create parser: %v", err)
	}
	for {
		v, err := p.Next()
		if err != nil {
			if err != io.EOF {
				t.Fatalf("Parser.Next failed: %v", err)
			}
			break
		}
		fmt.Printf("Parse: %v\n", v)
	}
}

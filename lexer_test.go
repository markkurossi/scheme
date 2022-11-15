//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
	"os"
	"testing"
)

func TestLexer(t *testing.T) {
	name := "testdata/wasm.scm"
	file, err := os.Open(name)
	if err != nil {
		t.Fatalf("failed to open test data '%s': %v", name, err)
	}
	defer file.Close()

	l := NewLexer(name, file)
	for {
		token, err := l.Get()
		if err != nil {
			if err == io.EOF {
				break
			}
			t.Fatalf("Lexer.Get: %v", err)
		}
		fmt.Printf("Token: %v\n", token)
	}
}

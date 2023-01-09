//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package main

import (
	"testing"

	"github.com/markkurossi/scheme"
)

func TestAPI(t *testing.T) {
	scm, err := scheme.New()
	if err != nil {
		t.Fatalf("scheme.New(): %v", err)
	}
	_ = scm
}

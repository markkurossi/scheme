//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"testing"
)

func TestTestsuite(t *testing.T) {
	scm, err := New()
	if err != nil {
		t.Fatalf("failed to create scheme: %v", err)
	}
	v, err := scm.EvalFile("testsuite/test.scm")
	if err != nil {
		t.Fatalf("eval testsuite failed: %v", err)
	}
	b, ok := IsBoolean(v)
	if !ok {
		t.Fatalf("invalid result %v, expected bool", v)
	}
	if !b {
		t.Errorf("testsuite failed")
	}
}

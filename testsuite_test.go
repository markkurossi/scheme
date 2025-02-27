//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"testing"
)

type logger struct {
	t    *testing.T
	line []byte
}

// Write implements io.Writer.
func (l *logger) Write(p []byte) (n int, err error) {
	for _, b := range p {
		if b == '\n' {
			l.t.Log(string(l.line))
			l.line = l.line[:0]
		} else {
			l.line = append(l.line, b)
		}
	}

	return len(p), nil
}

func TestTestsuite(t *testing.T) {
	scm, err := New()
	if err != nil {
		t.Fatalf("failed to create scheme: %v", err)
	}
	log := NewPort(&logger{
		t: t,
	})
	scm.Stdout = log
	scm.Stderr = log

	v, err := scm.EvalFile("testdata/test.scm")
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

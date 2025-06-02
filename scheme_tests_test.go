//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"strings"
	"testing"
)

func TestSchemeTests(t *testing.T) {
	scm := newTestScheme(t)

	lambda, err := scm.Eval("{input}", strings.NewReader(`(import (scheme test))
scheme::test::runner
`))
	if err != nil {
		t.Fatal(err)
	}
	args := []Value{
		Boolean(testing.Verbose()),
		String("./..."),
	}
	v, err := scm.Apply(lambda, args)
	if err != nil {
		t.Error(err)
	}
	bv, ok := v.(Boolean)
	if !ok {
		t.Errorf("unexpected result: %v", v)
	}
	if bool(bv) {
		t.Error("scheme tests failed")
	}
}

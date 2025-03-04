//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"testing"

	"github.com/markkurossi/scheme/types"
)

var inferenceTests = []struct {
	d string
	t *types.Type
}{
	{
		d: `#t`,
		t: types.Boolean,
	},
	{
		d: `1`,
		t: types.InexactInteger,
	},
	{
		d: `1.0`,
		t: types.InexactFloat,
	},
	{
		d: `#e1`,
		t: types.ExactInteger,
	},
	{
		d: `#e1.0`,
		t: types.ExactFloat,
	},
	{
		d: `#\t`,
		t: types.Character,
	},
	{
		d: `"Hello, world!"`,
		t: types.String,
	},
	{
		d: `(define limit 1)`,
		t: types.InexactInteger,
	},
	{
		d: `(define limit 1) limit`,
		t: types.InexactInteger,
	},

	{
		d: `(let ((x 1)) x)`,
		t: types.InexactInteger,
	},
}

func TestInference(t *testing.T) {
	for idx, test := range inferenceTests {
		name := fmt.Sprintf("test-%d", idx)
		scm := newTestScheme(t)

		library, err := scm.Load(name, strings.NewReader(test.d))
		if err != nil {
			t.Fatal(err)
		}
		values, ok := ListValues(library)
		if !ok || len(values) != 5 {
			t.Fatalf("%s: invalid library: %v", name, library)
			continue
		}
		lib, ok := values[4].(*Library)
		if !ok {
			t.Fatalf("%s: invalid library: %v", name, library)
		}
		_, typ, err := lib.Body.Infer(NewInferEnv(scm))
		if err != nil {
			t.Logf("%s: infer error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Fatalf(" \u2570 %s", err)
		}
		if !typ.IsA(test.t) {
			t.Logf("%s: type error in:", name)
			t.Logf(" \u2502 %s", test.d)
			t.Errorf(" \u2570 expected %v, got %v", test.t, typ)
		}
	}
}

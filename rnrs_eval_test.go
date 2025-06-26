//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"testing"
)

var evalTests = []struct {
	i string
	o string
}{
	{
		i: `1`,
		o: `1`,
	},
	{
		i: `"Hello, world!"`,
		o: `"Hello, world!"`,
	},
	{
		i: `'a`,
		o: `a`,
	},
	{
		i: `'(a b)`,
		o: `(a b)`,
	},
	{
		i: "`(a b ,1)",
		o: `(a b 1)`,
	},
	{
		i: "`(a b ,@'(1 2 3))",
		o: `(a b 1 2 3)`,
	},
	{
		i: "items",
		o: `(1 2 3)`,
	},
	{
		i: "`(a b ,items)",
		o: `(a b (1 2 3))`,
	},
	{
		i: "`(a b ,@items)",
		o: `(a b 1 2 3)`,
	},
}

func TestEval(t *testing.T) {
	for idx, test := range evalTests {
		i, err := ParseSexpr(test.i)
		if err != nil {
			t.Errorf("test-%d: input: %v", idx, err)
			continue
		}
		o, err := ParseSexpr(test.o)
		if err != nil {
			t.Errorf("test-%d: output: %v", idx, err)
			continue
		}
		env := NewEvalEnv(nil)

		env.Set("items", List(Int(1), Int(2), Int(3)))

		result, err := Eval(i, env)
		if err != nil {
			t.Errorf("test-%d: eval: %v", idx, err)
			continue
		}
		if !result.Equal(o) {
			t.Errorf("test-%d: %v != %v", idx, result, o)
		}
	}
}

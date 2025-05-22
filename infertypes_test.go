//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

// go test -run TestInferTypes

package scheme

import (
	"testing"

	"github.com/markkurossi/scheme/types"
)

func TestInferTypes(t *testing.T) {
	scm := newTestScheme(t)
	inferer := NewInferer(scm, nil)
	env := inferer.NewEnv()

	ast := &ASTConstant{}

	it := &InferTypes{
		Conclusive: true,
	}
	it.Add(types.InexactInteger)

	err := it.Learn(ast, env, types.Number)
	if err != nil {
		t.Errorf("%v.Learn(%v) failed: %v", it, types.Number, err)
	}
	err = it.Learn(ast, env, types.String)
	if err == nil {
		t.Errorf("learned more about conclusive type %v with %v",
			it, types.String)
	}

	it = &InferTypes{
		Conclusive: true,
	}
	it.Add(types.Number)
	err = it.Learn(ast, env, types.InexactInteger)
	if err != nil {
		t.Errorf("%v.Learn(%v) failed: %v", it, types.InexactInteger, err)
	}
}

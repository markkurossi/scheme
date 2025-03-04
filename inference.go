//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

// InferEnv implements type environment for Infer.
type InferEnv struct {
	scm      *Scheme
	bindings map[string]*types.Type
}

func NewInferEnv(scm *Scheme) *InferEnv {
	return &InferEnv{
		scm:      scm,
		bindings: make(map[string]*types.Type),
	}
}

func (env *InferEnv) Lookup(name string) *types.Type {
	t, ok := env.bindings[name]
	if ok {
		return t
	}
	return env.scm.Intern(name).GlobalType
}

// Subst implements type substitutions.
type InferSubst map[int]*types.Type

// Apply applies substitution for the type and returns the result
// type.
func (s InferSubst) Apply(t *types.Type) *types.Type {
	// switch t := t.(type) {
	// case TypeVar:
	// 	if replacement, ok := s[t.id]; ok {
	// 		return replacement
	// 	}
	// 	return t
	// case TypeArrow:
	// 	return TypeArrow{s.Apply(t.left), s.Apply(t.right)}
	// default:
	// 	return t
	// }
	return t
}

// Compose composes type substitutions s and s2.
func (s InferSubst) Compose(s2 InferSubst) InferSubst {
	result := make(InferSubst)
	for id, t := range s2 {
		result[id] = s.Apply(t)
	}
	for id, t := range s {
		if _, ok := result[id]; !ok {
			result[id] = t
		}
	}
	return result
}

// ApplyEnv applies substitutions for the environment and returns a
// new environment.
func (s InferSubst) ApplyEnv(env *InferEnv) *InferEnv {
	result := NewInferEnv(env.scm)
	for k, v := range env.bindings {
		result.bindings[k] = s.Apply(v)
	}
	return result
}

// Infer implements AST.Infer.
func (ast *ASTSequence) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	var subst InferSubst
	var t *types.Type
	var err error

	for _, item := range ast.Items {
		subst, t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env = subst.ApplyEnv(env)
	}
	return subst, t, nil
}

// Infer implements AST.Infer.
func (ast *ASTDefine) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Infer initializer type.
	_, t, err := ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	// Check the current type of the name.
	sym := env.scm.Intern(ast.Name.Name)
	if !sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, ast.From.Errorf("redefining symbol '%s'",
			ast.Name.Name)
	}

	sym.GlobalType = t

	return make(InferSubst), t, nil
}

// Infer implements AST.Infer.
func (ast *ASTSet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTSet.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTLet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTLet.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTIf) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTIf.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTApply) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTApply.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTCall) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTCall.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTCallUnary) Infer(env *InferEnv) (
	InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTCallUnary.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTLambda) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTLambda.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTConstant) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	subst := make(InferSubst)
	if ast.Value == nil {
		return subst, types.Nil, nil
	}
	return subst, ast.Value.Type(), nil
}

// Infer implements AST.Infer.
func (ast *ASTIdentifier) Infer(env *InferEnv) (
	InferSubst, *types.Type, error) {
	t := env.Lookup(ast.Name)
	if t.IsA(types.Unspecified) {
		fmt.Printf("ASTIdentifier: %s\n", ast.From)
		return nil, nil, ast.From.Errorf("undefined symbol '%s'", ast.Name)
	}
	return make(InferSubst), t, nil
}

// Infer implements AST.Infer.
func (ast *ASTCond) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTCond.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTCase) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTCase.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTAnd) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTAnd.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTOr) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTOr.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTPragma) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTPragma.Infer not implemented yet")
}

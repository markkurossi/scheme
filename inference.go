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

func (scm *Scheme) newTypeVar() *types.Type {
	i := scm.nextTypeVar
	scm.nextTypeVar++

	return &types.Type{
		Enum:    types.EnumTypeVar,
		TypeVar: i,
	}
}

// InferScheme implements a type with list of type variables.
type InferScheme struct {
	Variables []*types.Type
	Type      *types.Type
}

func (scheme *InferScheme) String() string {
	return fmt.Sprintf("Scheme(%v, %v)", scheme.Variables, scheme.Type)
}

// Instantiate creates a type of a type scheme.
func (scheme *InferScheme) Instantiate(scm *Scheme) *types.Type {
	// Replace all scheme.Variables with fresh TypeVar.
	result := scheme
	for _, v := range scheme.Variables {
		if v.Enum != types.EnumTypeVar {
			panic(fmt.Sprintf("invalid type variable: %v", v))
		}
		subst := make(InferSubst)
		subst[v.TypeVar] = &InferScheme{
			Type: scm.newTypeVar(),
		}
		result = subst.Apply(result)
	}
	return result.Type
}

// InferEnv implements type environment for Infer.
type InferEnv struct {
	scm      *Scheme
	bindings map[string]*InferScheme
}

// NewInferEnv creates a new inference environment.
func NewInferEnv(scm *Scheme) *InferEnv {
	return &InferEnv{
		scm:      scm,
		bindings: make(map[string]*InferScheme),
	}
}

// Copy creates a copy of the inference environment.
func (env *InferEnv) Copy() *InferEnv {
	result := NewInferEnv(env.scm)

	// XXX should we clone InferScheme too?

	for k, v := range env.bindings {
		result.bindings[k] = v
	}
	return result
}

// Lookup find the name's type from the environment.
func (env *InferEnv) Lookup(name string) *types.Type {
	t, ok := env.bindings[name]
	if ok {
		return t.Instantiate(env.scm)
	}
	return env.scm.Intern(name).GlobalType
}

// Generalize generalizes the type into a type scheme.
func (env *InferEnv) Generalize(t *types.Type) *InferScheme {
	return &InferScheme{
		Type: t,
	}
}

// InferSubst implements type substitutions.
type InferSubst map[int]*InferScheme

// Apply applies substitution for the type and returns the result
// type.
func (s InferSubst) Apply(t *InferScheme) *InferScheme {
	// Replace any variable that appears in the scheme's variable
	// list.
	result := &InferScheme{
		Type: t.Type,
	}

	for _, v := range t.Variables {
		if v.Enum != types.EnumTypeVar {
			panic(fmt.Sprintf("InferSubst.Apply: invalid type variable %v", v))
		}
		replacement, ok := s[v.TypeVar]
		if !ok {
			continue
		}
		switch result.Type.Enum {
		case types.EnumLambda:
			panic("InferSubst.Apply: Lambda not implemented yet")
		case types.EnumTypeVar:
			if v.IsA(result.Type) {
				result.Type = replacement.Type
			}
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

// Infer implements AST.Infer.
func (ast *ASTSequence) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	var subst InferSubst
	var err error

	for _, item := range ast.Items {
		subst, ast.t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env = subst.ApplyEnv(env)
	}
	return subst, ast.t, nil
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
	ast.t = t

	return make(InferSubst), t, nil
}

// Infer implements AST.Infer.
func (ast *ASTSet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	return nil, nil, fmt.Errorf("ASTSet.Infer not implemented yet")
}

// Infer implements AST.Infer.
func (ast *ASTLet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {

	bodyEnv := NewInferEnv(env.scm)
	var subst InferSubst
	var err error

	for _, b := range ast.Bindings {
		var type1 *types.Type
		subst, type1, err = b.Init.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env1 := subst.ApplyEnv(env)

		t := env1.Generalize(type1)

		bodyEnv.bindings[b.Name()] = t
		bodyEnv = subst.ApplyEnv(bodyEnv)
	}

	for _, body := range ast.Body {
		var s InferSubst
		s, ast.t, err = body.Infer(bodyEnv)
		if err != nil {
			return nil, nil, err
		}
		subst = subst.Compose(s)
	}
	return subst, ast.t, nil
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
	env = env.Copy()

	var args []*InferScheme
	for _, arg := range ast.Args.Fixed {
		scheme := env.Generalize(env.scm.newTypeVar())
		args = append(args, scheme)
		env.bindings[arg.Name] = scheme
	}
	if ast.Args.Rest != nil {
		scheme := env.Generalize(env.scm.newTypeVar())
		args = append(args, scheme)
		env.bindings[ast.Args.Rest.Name] = scheme
	}

	var subst InferSubst
	var t *types.Type
	var err error

	for _, item := range ast.Body {
		subst, t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env = subst.ApplyEnv(env)
	}

	// Apply substitution to argument types.
	for idx, arg := range args {
		args[idx] = subst.Apply(arg)
	}

	ast.t = &types.Type{
		Enum:   types.EnumLambda,
		Return: t,
	}
	for i := 0; i < len(ast.Args.Fixed); i++ {
		ast.t.Args = append(ast.t.Args, args[i].Type)
	}
	if ast.Args.Rest != nil {
		ast.t.Rest = args[len(ast.Args.Fixed)].Type
	}

	return subst, ast.t, nil
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
	ast.t = env.Lookup(ast.Name)
	if ast.t.IsA(types.Unspecified) {
		return nil, nil, ast.From.Errorf("undefined symbol '%s'", ast.Name)
	}
	return make(InferSubst), ast.t, nil
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

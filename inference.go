//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"unicode"

	"github.com/markkurossi/scheme/types"
)

// Inferer implements type inference.
type Inferer struct {
	scm     *Scheme
	defines []AST
	calls   map[*ASTLambda]*inferred
}

type inferred struct {
	subst InferSubst
	t     *types.Type
}

// NewInferer creates a new type inferer.
func NewInferer(scm *Scheme, toplevel []AST) *Inferer {
	inferer := &Inferer{
		scm:   scm,
		calls: make(map[*ASTLambda]*inferred),
	}
	for _, item := range toplevel {
		switch ast := item.(type) {
		case *ASTDefine:
			inferer.defines = append(inferer.defines, ast)

		case *ASTLambda:
			if ast.Define {
				inferer.defines = append(inferer.defines, ast)
			}
		}
	}
	return inferer
}

// Debugf prints debugging information about type inference.
func (inferer *Inferer) Debugf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.pragmaVerboseTypecheck {
		return
	}
	msg := fmt.Sprintf(format, a...)
	if len(msg) > 0 && unicode.IsSpace(rune(msg[0])) {
		msg = "  " + msg
	} else {
		msg = "\u22a2 " + msg
	}
	ast.Locator().From().Infof("%s", msg)
}

// NewEnv creates a new inference environment.
func (inferer *Inferer) NewEnv() *InferEnv {
	env := &InferEnv{
		inferer:  inferer,
		bindings: make(map[string]*InferScheme),
	}
	return env
}

func (inferer *Inferer) newTypeVar() *types.Type {
	i := inferer.scm.nextTypeVar
	inferer.scm.nextTypeVar++

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
func (scheme *InferScheme) Instantiate(inferer *Inferer) *types.Type {
	// Replace all scheme.Variables with fresh TypeVar.
	result := scheme
	for _, v := range scheme.Variables {
		if v.Enum != types.EnumTypeVar {
			panic(fmt.Sprintf("invalid type variable: %v", v))
		}
		subst := make(InferSubst)
		subst[v.TypeVar] = &InferScheme{
			Type: inferer.newTypeVar(),
		}
		result = subst.Apply(result)
	}
	return result.Type
}

// InferEnv implements type environment for Infer.
type InferEnv struct {
	inferer  *Inferer
	bindings map[string]*InferScheme
}

func (env *InferEnv) String() string {
	var result string

	for k, v := range env.bindings {
		if len(result) > 0 {
			result += ","
		}
		result += fmt.Sprintf("%v=%v", k, v)
	}
	return "[" + result + "]"
}

// Copy creates a copy of the inference environment.
func (env *InferEnv) Copy() *InferEnv {
	result := env.inferer.NewEnv()

	// XXX should we clone InferScheme too in bindings?

	for k, v := range env.bindings {
		result.bindings[k] = v
	}
	return result
}

// Lookup find the name's type from the environment.
func (env *InferEnv) Lookup(name string) *types.Type {
	scheme, ok := env.bindings[name]
	if ok {
		return scheme.Instantiate(env.inferer)
	}
	t := env.inferer.scm.Intern(name).GlobalType
	if !t.IsA(types.Unspecified) {
		return t
	}

	// Lookup the name from the toplevel AST.
	for _, item := range env.inferer.defines {
		switch def := item.(type) {
		case *ASTDefine:
			if def.Name.Name == name {
				return env.resolve(def)
			}

		case *ASTLambda:
			if def.Name.Name == name {
				return env.resolve(def)
			}
		}
	}
	return types.Unspecified
}

func (env *InferEnv) resolve(ast AST) *types.Type {
	_, t, err := ast.Infer(env)
	if err != nil {
		return types.Unspecified
	}
	return t
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
			for i, arg := range result.Type.Args {
				result.Type.Args[i] = replace(arg, v, replacement.Type)
			}
			if result.Type.Rest != nil {
				result.Type.Rest = replace(result.Type.Rest, v,
					replacement.Type)
			}
			result.Type.Return = replace(result.Type.Return, v,
				replacement.Type)

		case types.EnumTypeVar:
			result.Type = replace(result.Type, v, replacement.Type)
		}
	}

	return result
}

func replace(t, v, r *types.Type) *types.Type {
	if v.IsA(t) {
		return r
	}
	return t
}

// ApplyEnv applies substitutions for the environment and returns a
// new environment.
func (s InferSubst) ApplyEnv(env *InferEnv) *InferEnv {
	result := env.Copy()
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

// Unify unifies type from to type to and returns the substitutions
// that must be done to from.
func Unify(from, to *types.Type) (InferSubst, *types.Type, error) {
	return unify(from, to, make(InferSubst))
}

func unify(from, to *types.Type, subst InferSubst) (
	InferSubst, *types.Type, error) {

	if from.IsKindOf(to) {
		return subst, from, nil
	}
	if from.Enum == types.EnumTypeVar {
		return unifyTypeVar(from, to, subst)
	} else if to.Enum == types.EnumTypeVar {
		return unifyTypeVar(to, from, subst)
	}
	switch from.Enum {
	case types.EnumLambda:
		if to.Enum != types.EnumLambda {
			return nil, nil, fmt.Errorf("can't unify %v with %v", from, to)
		}
		if len(from.Args) != len(to.Args) {
			return nil, nil, fmt.Errorf("different amount of arguments")
		}
		result := &types.Type{
			Enum: types.EnumLambda,
		}
		var t *types.Type
		var err error
		for idx, arg := range from.Args {
			subst, t, err = unify(arg, to.Args[idx], subst)
			if err != nil {
				return nil, nil, err
			}
			result.Args = append(result.Args, t)
		}
		subst, t, err = unify(from.Return, to.Return, subst)
		if err != nil {
			return nil, nil, err
		}
		result.Return = t
		return subst, result, nil

	default:
		return nil, nil, fmt.Errorf("can't unify %v with %v", from, to)
	}
}

func unifyTypeVar(tv, to *types.Type, subst InferSubst) (
	InferSubst, *types.Type, error) {

	old, ok := subst[tv.TypeVar]
	if ok && !old.Type.IsA(to) {
		return nil, nil, fmt.Errorf("unify, old %v != new %v", old, to)
	}
	subst[tv.TypeVar] = &InferScheme{
		Type: to,
	}
	return subst, to, nil
}

// Infer implements AST.Infer.
func (ast *ASTSequence) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	subst := make(InferSubst)
	var err error

	for _, item := range ast.Items {
		var s InferSubst
		s, ast.t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		// XX think this env + subst composition
		// XXX ??? env = subst.ApplyEnv(env)
		subst = subst.Compose(s)
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
	sym := env.inferer.scm.Intern(ast.Name.Name)
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

	bodyEnv := env.Copy()
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
	// Resolve the type of the function.

	var fnSubst InferSubst // XXX sub1
	var fnType *types.Type // XXX type1
	var err error

	// XXX inlining should happen only after we know the argument
	// types. I.e. if the LambdaImpl has Inlinable flag set and the
	// argument types match the function.
	if ast.Inline {
		fnSubst, fnType, err = ast.inlineFuncType(env)
		if err != nil {
			return nil, nil, err
		}
	} else {
		fnSubst, fnType, err = ast.Func.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		if fnType.Enum != types.EnumLambda {
			return nil, nil,
				ast.Func.Locator().From().Errorf("invalid function: %v", fnType)
		}
		al := len(fnType.Args)
		if al < len(ast.Args) {
			// Check if the last argument Kind is Rest.
			if al == 0 && fnType.Args[al-1].Kind != types.Rest {
				ast.From.Errorf("too many arguments got %v, need %v",
					len(ast.Args), al)
			}
			fnType = fnType.Clone()

			rest := fnType.Args[al-1]
			rest.Kind = types.Fixed

			for i := al; i < len(ast.Args); i++ {
				fnType.Args = append(fnType.Args, rest)
			}
		} else if al > len(ast.Args) {
			// Check if the extra argument Kind are Optional.
			for i := len(ast.Args); i < al; i++ {
				if fnType.Args[i].Kind != types.Optional {
					ast.From.Errorf("too few arguments got %v, need %v",
						len(ast.Args), al)
				}
			}
			fnType = fnType.Clone()
			fnType.Args = fnType.Args[:len(ast.Args)]
		}
	}

	subst, retType, callType, err := inferCall(ast, env, fnSubst, fnType,
		ast.Args)
	if err != nil {
		return nil, nil, ast.From.Errorf("%s", err)
	}

	// Set types for arguments.
	for idx, t := range callType.Args {
		ast.Args[idx].SetType(t)
	}
	ast.t = retType

	return subst, retType, nil
}

func inferCall(ast AST, env *InferEnv, fnSubst InferSubst, fnType *types.Type,
	args []AST) (InferSubst, *types.Type, *types.Type, error) {

	// Create a new type variable for the return type of the function.
	rv := env.inferer.newTypeVar()

	// Create a new type environment.
	env1 := fnSubst.ApplyEnv(env)

	// Infer argument types.
	argSubst := make(InferSubst) // XXX sub2
	var argTypes []*types.Type   // XXX type2
	for _, arg := range args {
		s, t, err := arg.Infer(env1)
		if err != nil {
			return nil, nil, nil, err
		}
		argTypes = append(argTypes, t)
		argSubst = argSubst.Compose(s)
	}

	// Apply argSubst to fnType.
	fnTypeSubst := argSubst.Apply(env.Generalize(fnType))
	env.inferer.Debugf(ast, "Call: fnTypeSubst=%v\n", fnTypeSubst)

	// Create a function type for the called function.
	callType := &types.Type{ // XXX type3
		Enum:   types.EnumLambda,
		Args:   argTypes,
		Return: rv,
	}
	env.inferer.Debugf(ast, "Call: calltype=%v, unify:\n", callType)
	env.inferer.Debugf(ast, " \u256d\u2574 %v\n", callType)
	env.inferer.Debugf(ast, " \u251c\u2574 %v\n", fnTypeSubst.Type)

	// Unify fnTypeSubst with callType.
	subst3, unified, err := Unify(callType, fnTypeSubst.Type)
	if err != nil {
		fmt.Printf("Unify failed: %s:\n - %v\n - %v\n",
			err, callType, fnTypeSubst.Type)

		return nil, nil, nil, err
	}
	env.inferer.Debugf(ast, " \u2570> %v\n", unified)

	// Compose result substitutions.
	subst := fnSubst.Compose(argSubst.Compose(subst3))

	// Apply compositions to return variable.
	retType := subst.Apply(&InferScheme{
		Variables: []*types.Type{rv},
		Type:      rv,
	}).Type

	return subst, retType, unified, nil
}

func (ast *ASTCall) inlineFuncType(env *InferEnv) (
	InferSubst, *types.Type, error) {

	switch ast.InlineOp {
	case OpAdd, OpSub, OpMul, OpDiv:
		subst := make(InferSubst)
		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: types.Number,
		}

		// XXX min argument count.

		for i := 0; i < len(ast.Args); i++ {
			result.Args = append(result.Args, types.Number)
		}

		return subst, result, nil

	// XXX OpCons
	// XXX OpEq, OpLt, OpGt, OpLe, OpGe

	default:
		return nil, nil, ast.From.Errorf("%s: infer not implemented yet",
			ast.InlineOp)
	}
}

// Infer implements AST.Infer.
func (ast *ASTCallUnary) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Resolve th etype of the function.

	fnSubst := make(InferSubst)
	var fnType *types.Type

	switch ast.Op {
	case OpAddConst, OpSubConst, OpMulConst:
		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Number},
			Return: types.Number,
		}

	default:
		return nil, nil, ast.From.Errorf("%s: infer not implemented yet",
			ast.Op)
	}

	subst, retType, callType, err := inferCall(ast, env, fnSubst, fnType, []AST{
		ast.Arg,
	})
	if err != nil {
		return nil, nil, err
	}
	ast.Arg.SetType(callType.Args[0])

	return subst, retType, nil
}

// Infer implements AST.Infer.
func (ast *ASTLambda) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	cached, ok := env.inferer.calls[ast]
	if ok {
		return cached.subst, cached.t, nil
	}
	subst := make(InferSubst)
	retScheme := &InferScheme{
		Type: &types.Type{
			Enum:   types.EnumLambda,
			Return: env.inferer.newTypeVar(),
		},
	}
	retScheme.Variables = append(retScheme.Variables, retScheme.Type.Return)

	env.inferer.calls[ast] = &inferred{
		subst: subst,
		t:     retScheme.Type,
	}

	env = env.Copy()

	var args []*InferScheme
	for _, arg := range ast.Args.Fixed {
		scheme := env.Generalize(env.inferer.newTypeVar())
		args = append(args, scheme)
		env.bindings[arg.Name] = scheme

		retScheme.Variables = append(retScheme.Variables, scheme.Type)
		retScheme.Type.Args = append(retScheme.Type.Args, scheme.Type)
	}
	if ast.Args.Rest != nil {
		scheme := env.Generalize(env.inferer.newTypeVar())
		args = append(args, scheme)
		env.bindings[ast.Args.Rest.Name] = scheme

		retScheme.Variables = append(retScheme.Variables, scheme.Type)
		retScheme.Type.Return = scheme.Type
	}

	var t *types.Type
	var err error

	for _, item := range ast.Body {
		var s InferSubst
		subst, t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env = subst.ApplyEnv(env) // XXX ???
		subst = subst.Compose(s)
	}
	subst[retScheme.Type.Return.TypeVar] = env.Generalize(t)

	// Apply substitution to return type.
	retScheme = subst.Apply(retScheme)

	if retScheme.Type.Return.Enum == types.EnumTypeVar {
		retScheme.Type.Return = types.Unspecified
		env.inferer.Debugf(ast, "Lambda: no return value => %v\n",
			retScheme.Type.Return)
	}

	ast.t = retScheme.Type

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
	for _, d := range ast.Directives {
		if len(d) != 2 {
			return nil, nil, ast.From.Errorf("invalid directive: %v", d)
		}
		id, ok := d[0].(*Identifier)
		if !ok {
			return nil, nil, ast.From.Errorf(
				"invalid directive '%v': expected identifier", d[0])
		}
		switch id.Name {
		case "verbose-typecheck":
			v, ok := d[1].(Boolean)
			if !ok {
				return nil, nil,
					ast.From.Errorf("pragma %s: invalid argument: %v", id, d[1])
			}
			env.inferer.scm.pragmaVerboseTypecheck = bool(v)

		default:
			return nil, nil, ast.From.Errorf("unknown pragma '%s'", id.Name)
		}
	}

	return make(InferSubst), types.Unspecified, nil
}

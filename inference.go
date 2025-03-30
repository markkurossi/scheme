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

// Warningf prints a warning message about type inference.
func (inferer *Inferer) Warningf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.Verbose {
		return
	}
	msg := fmt.Sprintf(format, a...)
	if len(msg) > 0 && unicode.IsSpace(rune(msg[0])) {
		msg = "  " + msg
	} else {
		msg = "\u22a2 warning: " + msg
	}
	ast.Locator().From().Infof("%s", msg)
}

// Debugf prints debugging information about type inference.
func (inferer *Inferer) Debugf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.PragmaVerboseTypecheck {
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
		bindings: make(map[string]InferEnvBinding),
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
	bindings map[string]InferEnvBinding
	argTypes []*types.Type
	argSubst InferSubst
}

// InferEnvBinding defines known bindings in the inference
// environment. In most cases these contain the resolved InferSchemes
// but letrec environments contain forward declarations to initializer
// ASTs.
type InferEnvBinding struct {
	scheme *InferScheme
	ast    AST
}

func (ieb InferEnvBinding) String() string {
	if ieb.scheme != nil {
		return ieb.scheme.String()
	}
	return fmt.Sprintf("%v", ieb.ast)
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

// Get gets the name's type from the environment.
func (env *InferEnv) Get(name string) *types.Type {
	binding, ok := env.bindings[name]
	if ok {
		if binding.scheme != nil {
			return binding.scheme.Instantiate(env.inferer)
		}
		return env.resolve(binding.ast)
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

// Define defines the name with its type scheme.
func (env *InferEnv) Define(name string, scheme *InferScheme) error {
	_, ok := env.bindings[name]
	if ok {
		return fmt.Errorf("symbol already bound '%s'", name)
	}
	env.bindings[name] = InferEnvBinding{
		scheme: scheme,
	}
	return nil
}

// DefineAST defines the name with its initializer AST.
func (env *InferEnv) DefineAST(name string, ast AST) error {
	_, ok := env.bindings[name]
	if ok {
		return fmt.Errorf("symbol already bound '%s'", name)
	}
	env.bindings[name] = InferEnvBinding{
		ast: ast,
	}
	return nil
}

// Set sets the name's type scheme in the environment. This overrides
// any old binding the name might have.
func (env *InferEnv) Set(name string, scheme *InferScheme) {
	env.bindings[name] = InferEnvBinding{
		scheme: scheme,
	}
}

// SetAST sets the name's initializer AST in the environment. This
// overrides any old binding the name might have.
func (env *InferEnv) SetAST(name string, ast AST) {
	env.bindings[name] = InferEnvBinding{
		ast: ast,
	}
}

func (env *InferEnv) resolve(ast AST) *types.Type {
	_, t, err := ast.Infer(env)
	if err != nil {
		fmt.Printf("%s\n", err)
		return types.Unspecified
	}
	return t
}

// Generalize generalizes the type into a type scheme.
func (env *InferEnv) Generalize(t *types.Type) *InferScheme {
	if t == nil {
		panic("InferEnv.Generalize: nil type")
	}
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
	if t.Type == nil {
		panic("InferSubst.Apply: nil type")
	}
	result := &InferScheme{
		Variables: t.Variables,
		Type:      t.Type,
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
			if result.Type.Return == nil {
				panic("Lambda return type is nil")
			}

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
		if v.scheme != nil {
			result.bindings[k] = InferEnvBinding{
				scheme: s.Apply(v.scheme),
			}
		} else {
			result.bindings[k] = InferEnvBinding{
				ast: v.ast,
			}
		}
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
	if to == nil {
		panic("unifyTypeVar: to=nil")
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
		env = s.ApplyEnv(env)
		subst = subst.Compose(s)
	}
	return subst, ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTDefine) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Infer initializer type.
	subst, t, err := ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	// Check the current type of the name.
	sym := env.inferer.scm.Intern(ast.Name.Name)
	if !sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, ast.From.Errorf("redefining symbol '%s'", ast.Name)
	}

	sym.GlobalType = t
	ast.t = t

	return subst, t, nil
}

// Infer implements AST.Infer.
func (ast *ASTSet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Infer initializer type.
	subst, t, err := ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	if ast.Binding != nil {
		// let-variables can be assigned with different value types.
		env.Set(ast.Name, env.Generalize(t))
		ast.t = t
		return subst, ast.t, nil
	}

	// Check the current type of the name.
	sym := env.inferer.scm.Intern(ast.Name)
	if sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, ast.From.Errorf("\u22a2 setting undefined symbol '%s'",
			ast.Name)
	}
	if !t.IsKindOf(sym.GlobalType) {
		return nil, nil,
			ast.From.Errorf("can't assign %s to variable of type %s",
				t, sym.GlobalType)
	}

	ast.t = sym.GlobalType

	return subst, ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTLet) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	initEnv := env.Copy()
	bodyEnv := env.Copy()
	var subst InferSubst
	var err error

	// Bind all names to their init ASTs with letrec.
	if ast.Kind == KwLetrec {
		for _, b := range ast.Bindings {
			initEnv.SetAST(b.Name(), b.Init)
		}
	}

	// Resolve initializer types.
	for _, b := range ast.Bindings {
		var t *types.Type
		subst, t, err = b.Init.Infer(initEnv)
		if err != nil {
			return nil, nil, err
		}
		if ast.Kind != KwLet {
			initEnv.Set(b.Name(), initEnv.Generalize(t))
			initEnv = subst.ApplyEnv(initEnv)
		}

		bodyEnv.Set(b.Name(), initEnv.Generalize(t))
		bodyEnv = subst.ApplyEnv(bodyEnv)
	}

	for _, body := range ast.Body {
		var s InferSubst
		s, ast.t, err = body.Infer(bodyEnv)
		if err != nil {
			return nil, nil, err
		}
		bodyEnv = s.ApplyEnv(bodyEnv)
		subst = subst.Compose(s)
	}
	return subst, ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTIf) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	subst, t, err := ast.Cond.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	if t.Concrete() && !t.IsA(types.Boolean) {
		ast.Cond.Locator().From().Warningf("if test is not boolean: %v\n", t)
	}
	var tt, ft *types.Type
	subst, tt, err = ast.True.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	if ast.False != nil {
		subst, ft, err = ast.False.Infer(env)
		if err != nil {
			return nil, nil, err
		}
	} else {
		ft = types.Boolean
	}
	ast.t = types.Unify(tt, ft)

	return subst, ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTApply) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Resolve the type of the function.
	fnSubst, fnType, err := ast.Lambda.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	env.inferer.Debugf(ast, "Apply: fnType=%v\n", fnType)
	if fnType.Concrete() && fnType.Enum != types.EnumLambda {
		return nil, nil, ast.From.Errorf("invalid function: %v", fnType)
	}

	return fnSubst, fnType.Return, nil
}

// Infer implements AST.Infer.
func (ast *ASTCall) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	// Resolve call argument types.
	env.inferer.Debugf(ast, "Call: argument types:\n")
	argSubst := make(InferSubst)
	var argTypes []*types.Type
	for idx, arg := range ast.Args {
		s, t, err := arg.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		argTypes = append(argTypes, t)
		argSubst = argSubst.Compose(s)
		env.inferer.Debugf(ast, " %v: %v\n", idx, t)
	}
	env.argTypes = argTypes
	env.argSubst = argSubst

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
		env.inferer.Debugf(ast, "Call: inline: fnType=%v\n", fnType)
	} else {
		fnSubst, fnType, err = ast.Func.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env.inferer.Debugf(ast, "Call: code: fnType=%v\n", fnType)

		switch fnType.Enum {
		case types.EnumTypeVar:
			env.inferer.Debugf(ast, "unspecified function %v => %v\n",
				fnType, types.Unspecified)
			return make(InferSubst), types.Unspecified, nil

		case types.EnumLambda:

		default:
			return nil, nil,
				ast.Func.Locator().From().Errorf("invalid function: %v", fnType)
		}
		al := len(fnType.Args)
		if al < len(ast.Args) {
			// Check if the function supports rest arguments.

			fnType = fnType.Clone()
			var rest *types.Type

			if al > 0 && fnType.Args[al-1].Kind == types.Rest {
				rest = fnType.Args[al-1]
				rest.Kind = types.Fixed
			} else if fnType.Rest != nil {
				rest = fnType.Rest
				fnType.Rest = nil
			} else {
				return nil, nil,
					ast.From.Errorf("too many arguments got %v, need %v",
						len(ast.Args), al)
			}

			env.inferer.Debugf(ast, "%v expanding rest: %v\n", fnType, rest)

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

	env.inferer.Debugf(ast, "Call: fnType=%v\n", fnType)

	// Infer argument types.
	argSubst := make(InferSubst) // XXX sub2
	var argTypes []*types.Type   // XXX type2
	for idx, arg := range args {
		s, t, err := arg.Infer(env1)
		if err != nil {
			return nil, nil, nil, err
		}
		argTypes = append(argTypes, t)
		argSubst = argSubst.Compose(s)

		env.inferer.Debugf(ast, " %v: %v\n", idx, t)
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
		env.inferer.Warningf(ast, "Unify failed: %s:\n - %v\n - %v\n",
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

var minArgCount = map[Operand]int{
	OpAdd: 0,
	OpSub: 1,
	OpMul: 0,
	OpDiv: 1,

	OpEq: 2,
	OpLt: 2,
	OpGt: 2,
	OpLe: 2,
	OpGe: 2,

	OpCons: 2,
}

func (ast *ASTCall) inlineFuncType(env *InferEnv) (
	InferSubst, *types.Type, error) {

	subst := make(InferSubst)

	minArgs, ok := minArgCount[ast.InlineOp]
	if !ok {
		panic(fmt.Sprintf("unknown inline function: %v", ast.InlineOp))
	}
	if len(ast.Args) < minArgs {
		ast.From.Errorf("too few arguments: got %v, need %v",
			len(ast.Args), minArgs)
	}
	if ast.InlineOp == OpCons && len(ast.Args) > 2 {
		ast.From.Errorf("too many arguments: got %v, max 2", len(ast.Args))
	}

	env.inferer.Debugf(ast, "Call: inlineFuncType: %v\n", ast.InlineOp)

	switch ast.InlineOp {
	case OpAdd, OpSub, OpMul, OpDiv:
		var returnType *types.Type
		for idx, t := range env.argTypes {
			env.inferer.Debugf(ast, " %v: %v\n", idx, t)
			returnType = types.Unify(returnType, t)
		}
		if returnType == nil {
			returnType = types.Number
		}
		if returnType.Concrete() && !returnType.IsKindOf(types.Number) {
			return nil, nil,
				ast.From.Errorf("invalid arguments: %v, expected %v",
					returnType, types.Number)
		}

		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: returnType,
		}

		for i := 0; i < len(ast.Args); i++ {
			result.Args = append(result.Args, types.Number)
		}

		return subst, result, nil

	case OpCons:
		result := &types.Type{
			Enum: types.EnumLambda,
			Args: []*types.Type{types.Any, types.Any},
			Return: &types.Type{
				Enum: types.EnumPair,
				Car:  types.Any,
				Cdr:  types.Any,
			},
		}
		return subst, result, nil

	case OpEq, OpLt, OpGt, OpLe, OpGe:
		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: types.Boolean,
		}

		for i := 0; i < len(ast.Args); i++ {
			result.Args = append(result.Args, types.Number)
		}

		return subst, result, nil

	default:
		return nil, nil, ast.From.Errorf("%s: unknown inline function",
			ast.InlineOp)
	}
}

// Infer implements AST.Infer.
func (ast *ASTCallUnary) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	env.inferer.Debugf(ast, "CallUnary: %v\n", ast.Op)

	// Resolve argument type.
	_, argType, err := ast.Arg.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	// Resolve the type of the function.

	fnSubst := make(InferSubst)
	var fnType *types.Type

	switch ast.Op {
	case OpPairp, OpNullp, OpZerop, OpNot:
		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Any},
			Return: types.Boolean,
		}

	case OpAddConst, OpSubConst, OpMulConst:
		if argType.Concrete() {
			if !argType.IsKindOf(types.Number) {
				return nil, nil,
					ast.Arg.Locator().From().Errorf("invalid argument: %v",
						argType)
			}
		} else {
			argType = types.Number
		}

		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Number},
			Return: argType,
		}

	case OpCar, OpCdr:
		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Pair},
			Return: types.Unspecified,
		}

	case OpCastNumber:
		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Any},
			Return: types.Number,
		}

	case OpCastSymbol:
		fnType = &types.Type{
			Enum:   types.EnumLambda,
			Args:   []*types.Type{types.Any},
			Return: types.Symbol,
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
	ast.t = retType

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

	if ast.Define {
		id := env.inferer.scm.Intern(ast.Name.Name)
		id.GlobalType = retScheme.Type
	}

	env = env.Copy()

	var args []*InferScheme
	for _, arg := range ast.Args.Fixed {
		scheme := env.Generalize(env.inferer.newTypeVar())
		args = append(args, scheme)
		env.Set(arg.Name, scheme)

		retScheme.Variables = append(retScheme.Variables, scheme.Type)
		retScheme.Type.Args = append(retScheme.Type.Args, scheme.Type)
	}
	if ast.Args.Rest != nil {
		scheme := env.Generalize(env.inferer.newTypeVar())
		args = append(args, scheme)
		env.Set(ast.Args.Rest.Name, scheme)

		retScheme.Variables = append(retScheme.Variables, scheme.Type)
		retScheme.Type.Rest = scheme.Type
	}

	var t *types.Type
	var err error

	for _, item := range ast.Body {
		var s InferSubst
		s, t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env = s.ApplyEnv(env)
		subst = subst.Compose(s)
	}
	if t == nil {
		t = types.Unspecified
	}
	subst[retScheme.Type.Return.TypeVar] = env.Generalize(t)

	env.inferer.Debugf(ast, "\u03bb: return type: %v\n", retScheme)
	env.inferer.Debugf(ast, "\u03bb: subst: %v\n", subst)

	// Apply substitution to return type.
	retScheme = subst.Apply(retScheme)

	env.inferer.Debugf(ast, "\u03bb: return type: %v\n", retScheme)

	if retScheme.Type.Return.Enum == types.EnumTypeVar {
		retScheme.Type.Return = types.Any
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
	ast.t = env.Get(ast.Name)
	if ast.t.IsA(types.Unspecified) {
		return nil, nil, ast.From.Errorf("\u22a2 undefined symbol '%s'",
			ast.Name)
	}
	env.inferer.Debugf(ast, "Identifier: name=%v, t=%v\n", ast.Name, ast.t)
	if ast.t.Parametrizer != nil {
		env.inferer.Debugf(ast, "calling parametrizer\n")
		ctx := make(map[interface{}]bool)
		t, err := ast.t.Parametrizer.Parametrize(ctx, env.argTypes)
		if err != nil {
			return nil, nil, err
		}
		env.inferer.Debugf(ast, " => %v\n", t)
		ast.t = ast.t.Clone()
		ast.t.Return = t
	}

	return make(InferSubst), ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTCond) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	var result *types.Type
	var err error

	for _, choice := range ast.Choices {
		var choiceType *types.Type
		if choice.Cond == nil {
			choiceType = types.Boolean
		} else {
			_, choiceType, err = choice.Cond.Infer(env)
			if err != nil {
				return nil, nil, err
			}
			if choice.Func == nil && !choiceType.IsA(types.Boolean) {
				choice.Cond.Locator().From().
					Warningf("choice is not boolean: %v\n", choiceType)
			}
		}
		if choice.Func != nil {
			_, fnType, err := choice.Func.Infer(env)
			if err != nil {
				return nil, nil, err
			}
			// Check function validity.
			if fnType.Concrete() {
				if fnType.Enum != types.EnumLambda {
					return nil, nil, choice.Func.Locator().From().
						Errorf("invalid function: %v", fnType)
				}
				if fnType.MinArgs() != 1 {
					return nil, nil, choice.Func.Locator().From().
						Errorf("function must take one argument: %v", fnType)
				}
			}
			result = types.Unify(result, fnType.Return)
		} else if len(choice.Exprs) == 0 {
			result = types.Unify(result, choiceType)
		} else {
			choiceEnv := env.Copy()
			subst := make(InferSubst)
			var choiceType *types.Type

			for _, expr := range choice.Exprs {
				var s InferSubst
				s, choiceType, err = expr.Infer(choiceEnv)
				if err != nil {
					return nil, nil, err
				}
				choiceEnv = s.ApplyEnv(choiceEnv)
				subst = subst.Compose(s)
			}
			result = types.Unify(result, choiceType)
		}
	}
	ast.t = result

	return make(InferSubst), result, nil
}

// Infer implements AST.Infer.
func (ast *ASTCase) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	var result *types.Type

	_, _, err := ast.Expr.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	for _, choice := range ast.Choices {
		choiceEnv := env.Copy()
		subst := make(InferSubst)
		var choiceType *types.Type

		for _, expr := range choice.Exprs {
			var s InferSubst
			s, choiceType, err = expr.Infer(choiceEnv)
			if err != nil {
				return nil, nil, err
			}
			choiceEnv = s.ApplyEnv(choiceEnv)
			subst = subst.Compose(s)
		}
		result = types.Unify(result, choiceType)
	}
	ast.t = result

	return make(InferSubst), result, nil
}

// Infer implements AST.Infer.
func (ast *ASTAnd) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	subst := make(InferSubst)
	result := types.Boolean

	for _, expr := range ast.Exprs {
		_, t, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		if !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("and expr is not boolean: %v\n", t)
		}
		result = types.Unify(result, t)
	}
	ast.t = result

	return subst, ast.t, nil
}

// Infer implements AST.Infer.
func (ast *ASTOr) Infer(env *InferEnv) (InferSubst, *types.Type, error) {
	subst := make(InferSubst)
	result := types.Boolean

	for _, expr := range ast.Exprs {
		_, t, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		if !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("or expr is not boolean: %v\n", t)
		}
		result = types.Unify(result, t)
	}
	ast.t = result

	return subst, ast.t, nil
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
			env.inferer.scm.Params.PragmaVerboseTypecheck = bool(v)

		default:
			return nil, nil, ast.From.Errorf("unknown pragma '%s'", id.Name)
		}
	}

	return make(InferSubst), types.Unspecified, nil
}

//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/markkurossi/scheme/types"
)

func errf(ast AST, format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if ast.Locator() == nil {
		return fmt.Errorf("\u22a2 %s", msg)
	}
	return ast.Locator().From().Errorf("\u22a2 %s", msg)
}

// Inferer implements type inference.
type Inferer struct {
	scm      *Scheme
	defines  []AST
	nesting  int
	calls    map[AST]*types.Type
	inferred Inferred
}

// NewInferer creates a new type inferer.
func NewInferer(scm *Scheme, toplevel []AST) *Inferer {
	inferer := &Inferer{
		scm:      scm,
		calls:    make(map[AST]*types.Type),
		inferred: make(Inferred),
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

// Infer does type inference for the ast element.
func (inferer *Inferer) Infer(ast AST) (*types.Type, error) {
	_, _, err := ast.Infer(inferer.NewEnv())
	if err != nil {
		return nil, err
	}
	err = ast.Inferred(inferer.inferred)
	if err != nil {
		return nil, err
	}
	return ast.Type(), nil
}

// Enter increases inferer debug nesting.
func (inferer *Inferer) Enter(ast AST) {
	inferer.Debugf(ast, "\u256d\u2574%s.Enter\n", inferer.astName(ast))
	inferer.nesting++
}

// Exit decreases inferer debug nesting.
func (inferer *Inferer) Exit(ast AST) {
	inferer.nesting--
	inferer.Debugf(ast, "\u2570\u2574%s.Exit\n", inferer.astName(ast))
}

func (inferer *Inferer) astName(ast AST) string {
	name := fmt.Sprintf("%T", ast)
	idx := strings.IndexByte(name, '.')
	if idx > 0 {
		return name[idx+1:]
	}
	return name
}

// Print prints an inferer debug message.
func (inferer *Inferer) Print(ast AST, lead, msg string) {
	prefix := infererPrefix(ast.Locator())
	indent := "\u2502 "

	for i := 0; i < inferer.nesting; i++ {
		prefix += indent
	}
	inferer.scm.Stdout.Printf("%s%s%s", prefix, lead, msg)
}

func infererPrefix(loc Locator) string {
	var prefix string

	if loc != nil {
		var pad int
		p := loc.From()
		if p.Line < 10 {
			pad += 2
		} else if p.Line < 100 {
			pad++
		}
		if p.Col < 10 {
			pad++
		}

		prefix = fmt.Sprintf("%s:%d:%d:", p.Source, p.Line, p.Col)
		for i := 0; i < pad; i++ {
			prefix += " "
		}
	}

	return prefix + " \u22a2 "
}

// Warningf prints a warning message about type inference.
func (inferer *Inferer) Warningf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.Verbose {
		return
	}
	msg := fmt.Sprintf(format, a...)
	var lead string
	if len(msg) > 0 && unicode.IsSpace(rune(msg[0])) {
		lead = ""
	} else {
		lead = "warning: "
	}
	inferer.Print(ast, lead, msg)
}

// Debugf prints debugging information about type inference.
func (inferer *Inferer) Debugf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.Pragma.VerboseTypecheck {
		return
	}
	msg := fmt.Sprintf(format, a...)
	inferer.Print(ast, "", msg)
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

// Inferred stores type inference results.
type Inferred map[int]*types.Type

func (inferred Inferred) String() string {
	result := "["
	first := true

	for k, v := range inferred {
		if first {
			first = false
		} else {
			result += ","
		}
		result += fmt.Sprintf("%v=%v", k, v)
	}
	return result + "]"
}

// Copy creates a copy of the Inferred.
func (inferred Inferred) Copy() Inferred {
	if inferred == nil {
		return nil
	}
	result := make(Inferred)
	for k, v := range inferred {
		result[k] = v
	}
	return result
}

// Merge merges the learned types from the argument Inferred into
// this Inferred.
func (inferred Inferred) Merge(o Inferred) {
	for k, v := range o {
		inferred[k] = types.Unify(inferred[k], v)
	}
}

// Learn updates the type variable v's type to t. The function panics
// if the variable v is not a type variable.
func (inferred Inferred) Learn(v, t *types.Type) error {
	if v.Enum != types.EnumTypeVar {
		panic("InferEnv.Learn: invalid variable")
	}
	old, ok := inferred[v.TypeVar]
	if ok {
		if old.IsKindOf(t) {
			// Old value is the same or more specific type.
			return nil
		}
		if !t.IsKindOf(old) {
			return fmt.Errorf("can't convert type %s to %s", old, t)
		}
	}
	inferred[v.TypeVar] = t

	return nil
}

// Apply applies the type inference results for the argument type.
func (inferred Inferred) Apply(t *types.Type) *types.Type {
	if t == nil {
		return nil
	}
	switch t.Enum {
	case types.EnumLambda:
		for idx, arg := range t.Args {
			t.Args[idx] = inferred.Apply(arg)
		}
		t.Rest = inferred.Apply(t.Rest)
		t.Return = inferred.Apply(t.Return)

	case types.EnumPair:
		t.Car = inferred.Apply(t.Car)
		t.Cdr = inferred.Apply(t.Cdr)

	case types.EnumVector:
		t.Element = inferred.Apply(t.Element)

	case types.EnumTypeVar:
		mapped, ok := inferred[t.TypeVar]
		if ok {
			return mapped
		}
	}

	return t
}

// InferEnv implements environment for type inference.
type InferEnv struct {
	inferer  *Inferer
	bindings map[string]InferEnvBinding
	inferred Inferred

	// XXX
	argTypes []*types.Type
}

func (env *InferEnv) String() string {
	result := "bindings=["
	first := true

	for k, v := range env.bindings {
		if first {
			first = false
		} else {
			result += ","
		}
		result += fmt.Sprintf("%v=%v", k, v)
	}
	result += "]"
	if env.inferred != nil {
		result += ", inferred="
		result += env.inferred.String()
	}

	return result
}

// Copy creates a copy of the InferEnv.
func (env *InferEnv) Copy() *InferEnv {
	result := &InferEnv{
		inferer:  env.inferer,
		bindings: make(map[string]InferEnvBinding),
	}
	for k, v := range env.bindings {
		result.bindings[k] = v
	}
	result.inferred = env.inferred
	return result
}

// Positive creates a copy of the InferEnv for a positive branch.
func (env *InferEnv) Positive(branch *Branch) *InferEnv {
	result := env.Copy()
	if result.inferred == nil {
		result.inferred = make(Inferred)
	} else {
		result.inferred = result.inferred.Copy()
	}
	if branch == nil || branch.pos == nil {
		return result
	}
	for k, v := range branch.pos.bindings {
		result.bindings[k] = v
	}

	return result
}

// Negative creates a copy of the InferEnv for a negative branch.
func (env *InferEnv) Negative(branch *Branch) *InferEnv {
	result := env.Copy()
	if result.inferred == nil {
		result.inferred = make(Inferred)
	} else {
		result.inferred = result.inferred.Copy()
	}
	if branch == nil || branch.neg == nil {
		return result
	}
	for k, v := range branch.pos.bindings {
		result.bindings[k] = v
	}

	return result
}

// Inferred returns the Inferred, used by this environment.
func (env *InferEnv) Inferred() Inferred {
	if env.inferred != nil {
		return env.inferred
	}
	return env.inferer.inferred
}

// Get gets the name's type from the environment.
func (env *InferEnv) Get(name string) (*types.Type, bool) {
	binding, ok := env.bindings[name]
	if ok {
		if binding.t != nil {
			return env.follow(binding.t)
		}
		return env.resolve(binding.ast)
	}
	t := env.inferer.scm.Intern(name).GlobalType
	if !t.IsA(types.Unspecified) {
		return env.follow(t)
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
	return types.Unspecified, false
}

func (env *InferEnv) follow(t *types.Type) (*types.Type, bool) {
	for t.Enum == types.EnumTypeVar {
		next, ok := env.inferer.inferred[t.TypeVar]
		if !ok || next.Enum != types.EnumTypeVar {
			break
		}
		t = next
	}
	return t, true
}

func (env *InferEnv) resolve(ast AST) (*types.Type, bool) {
	_, t, err := ast.Infer(env)
	if err != nil {
		fmt.Printf("%s\n", err)
		return types.Unspecified, false
	}
	return env.follow(t)
}

// Define defines the name with its type scheme.
func (env *InferEnv) Define(name string, t *types.Type) error {
	_, ok := env.bindings[name]
	if ok {
		return fmt.Errorf("symbol already bound '%s'", name)
	}
	env.bindings[name] = InferEnvBinding{
		t: t,
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

// Set sets the name's type in the environment. This overrides any old
// binding the name might have.
func (env *InferEnv) Set(name string, t *types.Type) {
	env.bindings[name] = InferEnvBinding{
		t: t,
	}
}

// SetAST sets the name's initializer AST in the environment. This
// overrides any old binding the name might have.
func (env *InferEnv) SetAST(name string, ast AST) {
	env.bindings[name] = InferEnvBinding{
		ast: ast,
	}
}

// Learn updates the type variable v's type to t. The function panics
// if the variable v is not a type variable.
func (env *InferEnv) Learn(v, t *types.Type) error {
	if env.inferred != nil {
		return env.inferred.Learn(v, t)
	}
	return env.inferer.inferred.Learn(v, t)
}

// Branch defines the branch condition specific types.
//
// XXX These must hold the Inferred for positive and negative and we
// must merge them when the branch is used. The branch learnings are
// valid only on the respective branches, not globally.
type Branch struct {
	pos *InferEnv
	neg *InferEnv
}

// InferEnvBinding defines known bindings in the inference
// environment. In most cases these contain the resolved types but
// letrec environments contain forward declarations to initializer
// ASTs.
type InferEnvBinding struct {
	t   *types.Type
	ast AST
}

func (ieb InferEnvBinding) String() string {
	if ieb.t != nil {
		return ieb.t.String()
	}
	return fmt.Sprintf("%v", ieb.ast)
}

// Unify unifies type from to type to and returns the resulting type.
func Unify(ast AST, env *InferEnv, from, to *types.Type) (*types.Type, error) {
	return unify(ast, env, from, to)
}

func unify(ast AST, env *InferEnv, from, to *types.Type) (*types.Type, error) {
	if from.Enum == types.EnumTypeVar {
		return unifyTypeVar(env, from, to)
	}
	if from.IsKindOf(to) {
		return from, nil
	}

	var t *types.Type
	var err error

	switch from.Enum {
	case types.EnumLambda:
		if to.Enum != from.Enum {
			return nil, fmt.Errorf("can't unify %v with %v", from, to)
		}
		if len(from.Args) < len(to.Args) {
			return nil, fmt.Errorf("too few arguments: got %v, need %v",
				len(from.Args), len(to.Args))
		}
		if len(from.Args) > len(to.Args) {
			return nil, fmt.Errorf("too many arguments: got %v, need %v",
				len(from.Args), len(to.Args))
		}
		result := &types.Type{
			Enum: types.EnumLambda,
		}
		for idx, arg := range from.Args {
			t, err = unify(ast, env, arg, to.Args[idx])
			if err != nil {
				return nil, fmt.Errorf("argument %d: %s", idx, err)
			}
			result.Args = append(result.Args, t)
		}

		t, err = unify(ast, env, from.Return, to.Return)
		if err != nil {
			return nil, err
		}
		result.Return = t

		return result, nil

	case types.EnumPair:
		if to.Enum != from.Enum {
			return nil, fmt.Errorf("can't unify %v with %v", from, to)
		}
		result := &types.Type{
			Enum: types.EnumPair,
		}
		t, err = unify(ast, env, from.Car, to.Car)
		if err != nil {
			return nil, err
		}
		result.Car = t

		t, err = unify(ast, env, from.Cdr, to.Cdr)
		if err != nil {
			return nil, err
		}
		result.Cdr = t

		return result, nil

	case types.EnumVector:
		if to.Enum != from.Enum {
			return nil, fmt.Errorf("can't unify %v with %v", from, to)
		}
		result := &types.Type{
			Enum: types.EnumVector,
		}
		t, err = unify(ast, env, from.Element, to.Element)
		if err != nil {
			return nil, err
		}
		result.Element = t

		return result, nil

	default:
		return nil, fmt.Errorf("can't unify %v with %v", from, to)
	}
}

func unifyTypeVar(env *InferEnv, tv, to *types.Type) (*types.Type, error) {
	if to == nil {
		panic("unifyTypeVar: to=nil")
	}
	err := env.Learn(tv, to)
	if err != nil {
		return nil, err
	}

	return to, nil
}

// Infer implements AST.Infer.
func (ast *ASTSequence) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var err error

	for _, item := range ast.Items {
		_, ast.t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
	}
	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTSequence) Inferred(inferred Inferred) error {
	for _, item := range ast.Items {
		err := item.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTDefine) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	cached, ok := env.inferer.calls[ast]
	if ok {
		return nil, cached, nil
	}

	// Infer initializer type.
	_, t, err := ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	// Check the current type of the name.
	sym := env.inferer.scm.Intern(ast.Name.Name)
	if !sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, errf(ast, "redefining symbol '%s'", ast.Name)
	}

	env.inferer.calls[ast] = t

	env.inferer.Debugf(ast, "%v=%v\n", ast.Name.Name, t)
	sym.GlobalType = t
	ast.t = t

	return nil, t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTDefine) Inferred(inferred Inferred) error {
	err := ast.Value.Inferred(inferred)
	if err != nil {
		return err
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTSet) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	// Infer initializer type.
	_, t, err := ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	if ast.Binding != nil {
		// let-variables can be assigned with different value types.
		env.Set(ast.Name, t)
		ast.t = t
		return nil, ast.t, nil
	}

	// Check the current type of the name.
	sym := env.inferer.scm.Intern(ast.Name)
	if sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, errf(ast, "setting undefined symbol '%s'", ast.Name)
	}

	if !t.IsKindOf(sym.GlobalType) {
		_, err := Unify(ast, env, t, sym.GlobalType)
		if err != nil {
			return nil, nil, errf(ast, "can't assign %s to variable of type %s",
				t, sym.GlobalType)
		}
	}

	ast.t = sym.GlobalType

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTSet) Inferred(inferred Inferred) error {
	err := ast.Value.Inferred(inferred)
	if err != nil {
		return err
	}
	ast.t = ast.Value.Type()

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTLet) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	initEnv := env.Copy()
	var err error

	// Bind all names to their init ASTs with letrec.
	if ast.Kind == KwLetrec {
		for _, b := range ast.Bindings {
			initEnv.SetAST(b.Name(), b.Init)
		}
	}

	bodyBindings := make(map[string]*types.Type)

	// Resolve initializer types.
	for _, b := range ast.Bindings {
		var t *types.Type
		_, t, err = b.Init.Infer(initEnv)
		if err != nil {
			return nil, nil, err
		}
		if ast.Kind == KwLet {
			bodyBindings[b.Name()] = t
		} else {
			initEnv.Set(b.Name(), t)
		}
	}

	bodyEnv := initEnv.Copy()
	for name, t := range bodyBindings {
		bodyEnv.Set(name, t)
	}

	for _, body := range ast.Body {
		_, ast.t, err = body.Infer(bodyEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTLet) Inferred(inferred Inferred) error {
	for _, b := range ast.Bindings {
		err := b.Init.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	for _, body := range ast.Body {
		err := body.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTIf) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	branch, t, err := ast.Cond.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
		t.Concrete() && !t.IsA(types.Boolean) {
		ast.Cond.Locator().From().Warningf("if test is not boolean: %v\n", t)
	}

	env.inferer.Debugf(ast, "If: branch=%v\n", branch)

	trueEnv := env.Positive(branch)
	_, _, err = ast.True.Infer(trueEnv)
	if err != nil {
		return nil, nil, err
	}

	if ast.False != nil {
		falseEnv := env.Negative(branch)
		_, _, err = ast.False.Infer(falseEnv)
		if err != nil {
			return nil, nil, err
		}

		// The if expression is conclusive. Merge learnings into
		// global curriculum.
		env.inferer.Debugf(ast, " => true  : %v\n", trueEnv.inferred)
		env.inferer.Debugf(ast, " => false : %v\n", falseEnv.inferred)
		trueEnv.inferred.Merge(falseEnv.inferred)
		env.inferer.Debugf(ast, " => merged: %v\n", trueEnv.inferred)

		env.Inferred().Merge(trueEnv.inferred)
	}

	err = ast.True.Inferred(env.Inferred())
	if err != nil {
		return nil, nil, err
	}
	tt := ast.True.Type()

	var ft *types.Type
	if ast.False != nil {
		err = ast.False.Inferred(env.Inferred())
		if err != nil {
			return nil, nil, err
		}
		ft = ast.False.Type()
	} else {
		ft = types.Boolean
	}

	ast.t = types.Unify(tt, ft)
	env.inferer.Debugf(ast, "If types.Unify(%v,%v)=>%v\n", tt, ft, ast.t)

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTIf) Inferred(inferred Inferred) error {
	err := ast.Cond.Inferred(inferred)
	if err != nil {
		return err
	}
	err = ast.True.Inferred(inferred)
	if err != nil {
		return err
	}
	if ast.False != nil {
		err = ast.False.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTApply) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	// Resolve the type of the function.
	_, fnType, err := ast.Lambda.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	env.inferer.Debugf(ast, "Apply: fnType=%v\n", fnType)
	if fnType.Concrete() && fnType.Enum != types.EnumLambda {
		return nil, nil, errf(ast, "invalid function: %v", fnType)
	}
	ast.t = fnType.Return

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTApply) Inferred(inferred Inferred) error {
	err := ast.Lambda.Inferred(inferred)
	if err != nil {
		return err
	}
	err = ast.Args.Inferred(inferred)
	if err != nil {
		return err
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

var typePredicates = map[string]*types.Type{
	"boolean?":    types.Boolean,
	"bytevector?": types.Bytevector,
	"char?":       types.Character,
	"float?":      types.InexactFloat,
	"integer?":    types.InexactInteger,
	"number?":     types.Number,
	"pair?":       types.Pair,
	"string?":     types.String,
	"symbol?":     types.Symbol,
	"vector?":     types.Vector,
}

// Infer implements AST.Infer.
func (ast *ASTCall) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env = env.Copy()

	// Resolve call argument types.
	env.inferer.Debugf(ast, "Call: argument types:\n")
	var argTypes []*types.Type
	for idx, arg := range ast.Args {
		_, t, err := arg.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		argTypes = append(argTypes, t)
		env.inferer.Debugf(ast, " %v: %v\n", idx, t)
	}
	env.argTypes = argTypes

	// Resolve the type of the function.

	var fnName string
	var fnType *types.Type // XXX type1
	var err error

	// XXX inlining should happen only after we know the argument
	// types. I.e. if the LambdaImpl has Inlinable flag set and the
	// argument types match the function.
	if ast.Inline {
		fnType, err = ast.inlineFuncType(env)
		if err != nil {
			return nil, nil, err
		}
		env.inferer.Debugf(ast, "Call: inline: fnType=%v\n", fnType)
		fnName = ast.InlineOp.String()
	} else {
		_, fnType, err = ast.Func.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		env.inferer.Debugf(ast, "Call: code: fnType=%v\n", fnType)
		switch fn := ast.Func.(type) {
		case *ASTIdentifier:
			fnName = fn.Name
		default:
			fnName = "lambda"
		}

		switch fnType.Enum {
		case types.EnumTypeVar:
			env.inferer.Debugf(ast, "unspecified function %v => %v\n",
				fnType, types.Unspecified)
			return nil, types.Unspecified, nil

		case types.EnumLambda:

		default:
			return nil, nil, errf(ast.Func, "invalid function: %v", fnType)
		}

		al := len(fnType.Args)
		if al < len(ast.Args) {
			// Check if the function supports rest arguments.

			fnType = fnType.Clone()
			var rest *types.Type

			if al > 0 && fnType.Args[al-1].Kind == types.Rest {
				rest = fnType.Args[al-1]
				rest.Kind = types.Fixed

				env.inferer.Debugf(ast, "%v expanding rest: %v\n", fnType, rest)

				for i := al; i < len(ast.Args); i++ {
					fnType.Args = append(fnType.Args, rest)
				}
			} else if fnType.Rest != nil {
				rest = fnType.Rest
				fnType.Rest = nil

				// Resolve the type of the rest list's head.
				env.inferer.Debugf(ast, "Resolving rest list's head type\n")
				var headType *types.Type
				for i := al; i < len(argTypes); i++ {
					env.inferer.Debugf(ast, " rest-%v: %v\n", i, argTypes[i])
					headType = types.Unify(headType, argTypes[i])
				}
				env.inferer.Debugf(ast, "Rest: cons(%v, ?)\n", headType)

				restType := &types.Type{
					Enum: types.EnumPair,
					Car:  headType,
					Cdr:  env.inferer.newTypeVar(),
				}

				env.argTypes = env.argTypes[:al]
				env.argTypes = append(env.argTypes, restType)
				env.inferer.Debugf(ast, "Rest: argTypes: %v\n", env.argTypes)

				fnType.Args = append(fnType.Args, restType)

			} else {
				return nil, nil, errf(ast, "too many arguments got %v, need %v",
					len(ast.Args), al)
			}

		} else if al > len(ast.Args) {
			// Check if the extra argument Kind are Optional.
			for i := len(ast.Args); i < al; i++ {
				if fnType.Args[i].Kind != types.Optional {
					errf(ast, "too few arguments got %v, need %v",
						len(ast.Args), al)
				}
			}
			fnType = fnType.Clone()
			fnType.Args = fnType.Args[:len(ast.Args)]
		}
	}

	retType, callType, err := inferCall(ast, env, fnType, ast.Args)
	if err != nil {
		return nil, nil, errf(ast, "%s: %s", fnName, err)
	}

	// Set types for arguments.
	for idx, t := range callType.Args {
		ast.Args[idx].SetType(t)
	}
	ast.t = retType

	// Check typecheck predicates.
	var branch *Branch
	id, ok := ast.Func.(*ASTIdentifier)
	if ok && len(fnType.Args) == 1 {
		t, ok := typePredicates[id.Name]
		if ok {
			argID, ok := ast.Args[0].(*ASTIdentifier)
			if ok {
				branch = &Branch{
					pos: env.inferer.NewEnv(),
				}
				env.inferer.Debugf(ast, "%s: %v=%v\n", id.Name, argID, t)
				branch.pos.bindings[argID.Name] = InferEnvBinding{
					t: t,
				}
			}
		}
	}

	return branch, retType, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCall) Inferred(inferred Inferred) error {
	var err error

	if ast.Func != nil {
		err = ast.Func.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	for _, arg := range ast.Args {
		err = arg.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)

	return nil
}

func inferCall(ast AST, env *InferEnv, fnType *types.Type, args []AST) (
	*types.Type, *types.Type, error) {

	// Create a new type variable for the return type of the function.
	rv := env.inferer.newTypeVar()

	// Create a new type environment.
	env1 := env.Copy()

	env.inferer.Debugf(ast, "Call: fnType=%v\n", fnType)

	// Infer argument types.
	var argTypes []*types.Type // XXX type2
	for idx, arg := range args {
		_, t, err := arg.Infer(env1)
		if err != nil {
			return nil, nil, err
		}
		argTypes = append(argTypes, t)

		env.inferer.Debugf(ast, " %v: %v\n", idx, t)
	}
	if len(env.argTypes) > 0 {
		argTypes = env.argTypes
	}

	// Create a function type for the called function.
	callType := &types.Type{ // XXX type3
		Enum:   types.EnumLambda,
		Args:   argTypes,
		Return: rv,
	}
	env.inferer.Debugf(ast, "Call: calltype=%v, unify:\n", callType)
	env.inferer.Debugf(ast, " \u256d\u2574 %v\n", callType)
	env.inferer.Debugf(ast, " \u251c\u2574 %v\n", fnType)

	// Unify fnTypeSubst with callType.
	unified, err := Unify(ast, env, callType, fnType)
	if err != nil {
		env.inferer.Warningf(ast, "Unify: %s:\n", err)
		env.inferer.Warningf(ast, " - %v\n", callType)
		env.inferer.Warningf(ast, " - %v\n", fnType)
		env.inferer.Warningf(ast, " - %v\n", env.inferer.inferred)
		return nil, nil, err
	}
	env.inferer.Debugf(ast, " \u2570> %v\n", unified)

	return rv, unified, nil
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

func (ast *ASTCall) inlineFuncType(env *InferEnv) (*types.Type, error) {

	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	minArgs, ok := minArgCount[ast.InlineOp]
	if !ok {
		panic(fmt.Sprintf("unknown inline function: %v", ast.InlineOp))
	}
	if len(ast.Args) < minArgs {
		return nil, errf(ast, "too few arguments: got %v, need %v",
			len(ast.Args), minArgs)
	}
	if ast.InlineOp == OpCons && len(ast.Args) > 2 {
		return nil, errf(ast, "too many arguments: got %v, need 2",
			len(ast.Args))
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
			return nil, errf(ast, "invalid arguments: %v, expected %v",
				returnType, types.Number)
		}

		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: returnType,
		}

		for i := 0; i < len(ast.Args); i++ {
			result.Args = append(result.Args, types.Number)
		}

		return result, nil

	case OpCons:
		result := &types.Type{
			Enum: types.EnumLambda,
			Args: []*types.Type{types.Any, types.Any},
			Return: &types.Type{
				Enum: types.EnumPair,
				Car:  env.argTypes[0],
				Cdr:  env.argTypes[1],
			},
		}
		return result, nil

	case OpEq, OpLt, OpGt, OpLe, OpGe:
		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: types.Boolean,
		}

		for i := 0; i < len(ast.Args); i++ {
			result.Args = append(result.Args, types.Number)
		}

		return result, nil

	default:
		return nil, errf(ast, "%s: unknown inline function", ast.InlineOp)
	}
}

// Infer implements AST.Infer.
func (ast *ASTCallUnary) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env.inferer.Debugf(ast, "CallUnary: %v\n", ast.Op)

	// Resolve argument type.
	_, argType, err := ast.Arg.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	env.argTypes = []*types.Type{argType}

	// Resolve the type of the function.

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
				return nil, nil, errf(ast.Arg, "invalid argument: %v", argType)
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

	default:
		return nil, nil, errf(ast, "%s: infer not implemented yet", ast.Op)
	}

	retType, callType, err := inferCall(ast, env, fnType, []AST{
		ast.Arg,
	})
	if err != nil {
		return nil, nil, err
	}
	ast.Arg.SetType(callType.Args[0])
	ast.t = retType

	// Check pair? predicate.
	var branch *Branch
	if ast.Op == OpPairp {
		id, ok := ast.Arg.(*ASTIdentifier)
		if ok {
			branch = &Branch{
				pos: env.inferer.NewEnv(),
			}
			t := &types.Type{
				Enum: types.EnumPair,
				Car:  types.Any,
				Cdr:  types.Any,
			}
			env.inferer.Debugf(ast, "%s %v=%v\n", ast.Op, id, t)
			branch.pos.bindings[id.Name] = InferEnvBinding{
				t: t,
			}
		}
	}

	return branch, retType, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCallUnary) Inferred(inferred Inferred) error {
	err := ast.Arg.Inferred(inferred)
	if err != nil {
		return err
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTLambda) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env.inferer.scm.Params.PushScope()
	defer env.inferer.scm.Params.PopScope()

	cached, ok := env.inferer.calls[ast]
	if ok {
		return nil, cached, nil
	}
	retType := &types.Type{
		Enum:   types.EnumLambda,
		Return: env.inferer.newTypeVar(),
	}
	env.inferer.calls[ast] = retType

	env = env.Copy()

	var args []*types.Type
	for idx, arg := range ast.Args.Fixed {
		t := env.inferer.newTypeVar()

		env.inferer.Debugf(ast, "\u03bb: arg[%v]=%v\n", idx, t)

		args = append(args, t)
		env.Set(arg.Name, t)

		retType.Args = append(retType.Args, t)
	}
	if ast.Args.Rest != nil {
		t := &types.Type{
			Enum: types.EnumPair,
			Car:  env.inferer.newTypeVar(),
			Cdr:  env.inferer.newTypeVar(),
		}
		args = append(args, t)
		env.Set(ast.Args.Rest.Name, t)

		retType.Rest = t
	}
	env.inferer.Debugf(ast, "\u03bb: env=%v\n", env)

	var t *types.Type
	var err error

	for _, item := range ast.Body {
		_, t, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
	}
	if t == nil {
		t = types.Unspecified
	}
	env.Learn(retType.Return, t)

	env.inferer.Debugf(ast, "\u03bb: return type: %v\n", retType)
	env.inferer.Debugf(ast, "\u03bb: inferred=%v\n", env.inferer.inferred)
	ast.t = retType

	ast.Inferred(env.inferer.inferred)

	if ast.Define {
		sym := env.inferer.scm.Intern(ast.Name.Name)
		if !sym.GlobalType.IsA(types.Unspecified) {
			return nil, nil, errf(ast, "redefining symbol '%s'", ast.Name)
		}
		sym.GlobalType = ast.t
		env.inferer.Debugf(ast, "%v=%v\n", ast.Name, ast.t)
	}

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTLambda) Inferred(inferred Inferred) error {
	for _, item := range ast.Body {
		err := item.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTConstant) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	if ast.Value == nil {
		return nil, types.Nil, nil
	}
	return nil, ast.Value.Type(), nil
}

// Inferred implements AST.Inferred.
func (ast *ASTConstant) Inferred(inferred Inferred) error {
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTIdentifier) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var ok bool
	ast.t, ok = env.Get(ast.Name)
	if !ok {
		return nil, nil, errf(ast, "undefined symbol '%s'", ast.Name)
	}
	env.inferer.Debugf(ast, "name=%v, t=%v\n", ast.Name, ast.t)
	if ast.t.Parametrizer != nil && len(env.argTypes) > 0 {
		env.inferer.Debugf(ast, "calling parametrizer\n")
		ctx := make(map[interface{}]bool)
		t, err := ast.t.Parametrizer.Parametrize(ctx, env.argTypes)
		if err != nil {
			return nil, nil, errf(ast, "%s", err)
		}
		env.inferer.Debugf(ast, " => %v\n", t)
		ast.t = ast.t.Clone()
		ast.t.Return = t
	}

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTIdentifier) Inferred(inferred Inferred) error {
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTCond) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var result *types.Type
	var err error

	for _, choice := range ast.Choices {
		var choiceBranch *Branch
		var choiceType *types.Type

		if choice.Cond == nil {
			choiceType = types.Boolean
		} else {
			choiceBranch, choiceType, err = choice.Cond.Infer(env)
			if err != nil {
				return nil, nil, err
			}
			if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
				choice.Func == nil && choiceType.Concrete() &&
				!choiceType.IsA(types.Boolean) {
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
					return nil, nil, errf(choice.Func, "invalid function: %v",
						fnType)
				}
				if fnType.MinArgs() != 1 {
					return nil, nil, errf(choice.Func,
						"function must take one argument: %v", fnType)
				}
			}
			result = types.Unify(result, fnType.Return)
		} else if len(choice.Exprs) == 0 {
			result = types.Unify(result, choiceType)
		} else {
			choiceEnv := env.Positive(choiceBranch)
			var choiceType *types.Type

			for _, expr := range choice.Exprs {
				_, _, err = expr.Infer(choiceEnv)
				if err != nil {
					return nil, nil, err
				}
			}
			for _, expr := range choice.Exprs {
				err = expr.Inferred(choiceEnv.Inferred())
				if err != nil {
					return nil, nil, err
				}
				choiceType = expr.Type()
			}
			tt := types.Unify(result, choiceType)
			env.inferer.Debugf(ast, "Cond types.Unify(%v,%v)=>%v\n",
				result, choiceType, tt)

			if choice.Cond == nil {
				// The cond expression is conclusive. Merge learnings
				// into global curriculum.
				env.Inferred().Merge(choiceEnv.inferred)
			}

			result = tt
		}
	}
	ast.t = result

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCond) Inferred(inferred Inferred) error {
	for _, choice := range ast.Choices {
		if choice.Cond != nil {
			err := choice.Cond.Inferred(inferred)
			if err != nil {
				return err
			}
		}
		if choice.Func != nil {
			err := choice.Func.Inferred(inferred)
			if err != nil {
				return nil
			}
		}
		for _, expr := range choice.Exprs {
			err := expr.Inferred(inferred)
			if err != nil {
				return err
			}
		}
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTCase) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var result *types.Type

	_, _, err := ast.Expr.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	for _, choice := range ast.Choices {
		choiceEnv := env.Copy()
		var choiceType *types.Type

		for _, expr := range choice.Exprs {
			_, choiceType, err = expr.Infer(choiceEnv)
			if err != nil {
				return nil, nil, err
			}
		}
		// XXX check tail call
		result = types.Unify(result, choiceType)
	}
	ast.t = result

	return nil, result, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCase) Inferred(inferred Inferred) error {
	err := ast.Expr.Inferred(inferred)
	if err != nil {
		return err
	}
	for _, choice := range ast.Choices {
		for _, expr := range choice.Exprs {
			err = expr.Inferred(inferred)
			if err != nil {
				return nil
			}
		}
	}
	ast.t = inferred.Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTAnd) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	result := types.Boolean

	for _, expr := range ast.Exprs {
		_, t, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
			t.Concrete() && !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("and expr is not boolean: %v\n", t)
		}
		result = types.Unify(result, t)
	}
	ast.t = result

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTAnd) Inferred(inferred Inferred) error {
	for _, expr := range ast.Exprs {
		err := expr.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTOr) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	result := types.Boolean

	for _, expr := range ast.Exprs {
		_, t, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
			t.Concrete() && !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("or expr is not boolean: %v\n", t)
		}
		result = types.Unify(result, t)
	}
	ast.t = result

	return nil, ast.t, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTOr) Inferred(inferred Inferred) error {
	for _, expr := range ast.Exprs {
		err := expr.Inferred(inferred)
		if err != nil {
			return err
		}
	}
	ast.t = inferred.Apply(ast.t)

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTPragma) Infer(env *InferEnv) (*Branch, *types.Type, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	for _, d := range ast.Directives {
		if len(d) != 2 {
			return nil, nil, errf(ast, "invalid directive: %v", d)
		}
		id, ok := d[0].(*Identifier)
		if !ok {
			return nil, nil,
				errf(ast, "invalid directive '%v': expected identifier", d[0])
		}
		switch id.Name {
		case "check-boolean-exprs":
			v, ok := d[1].(Boolean)
			if !ok {
				return nil, nil, errf(ast, "pragma %s: invalid argument: %v",
					id, d[1])
			}
			env.inferer.scm.Params.Pragma.NoCheckBooleanExprs = !bool(v)

		case "verbose-typecheck":
			v, ok := d[1].(Boolean)
			if !ok {
				return nil, nil, errf(ast, "pragma %s: invalid argument: %v",
					id, d[1])
			}
			env.inferer.scm.Params.Pragma.VerboseTypecheck = bool(v)

		default:
			return nil, nil, errf(ast, "unknown pragma '%s'", id.Name)
		}
	}

	return nil, types.Unspecified, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTPragma) Inferred(inferred Inferred) error {
	return nil
}

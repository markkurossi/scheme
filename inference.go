//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"sort"
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

// Inferer
// Inferred
// InferEnv
// InferEnvBinding
// InferBranch
// InferTypes

// Inferer implements type inference.
type Inferer struct {
	scm      *Scheme
	defines  []AST
	nesting  int
	calls    map[AST]*InferTypes
	inferred Inferred
}

// NewInferer creates a new type inferer.
func NewInferer(scm *Scheme, toplevel []AST) *Inferer {
	inferer := &Inferer{
		scm:      scm,
		calls:    make(map[AST]*InferTypes),
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
	env := inferer.NewEnv()
	_, _, err := ast.Infer(env)
	if err != nil {
		return nil, err
	}
	err = ast.Inferred(env)
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
	msg := fmt.Sprintf(format, a...)
	var lead string
	if len(msg) > 0 && unicode.IsSpace(rune(msg[0])) {
		lead = ""
	} else {
		lead = "warning: "
	}
	if inferer.scm.Params.Verbose() {
		inferer.Print(ast, lead, msg)
	} else {
		inferer.scm.Stdout.Printf("%s: %s%s", ast.Locator().From(), lead, msg)
	}
}

// Debugf prints debugging information about type inference.
func (inferer *Inferer) Debugf(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.Pragma.VerboseTypecheck {
		return
	}
	msg := fmt.Sprintf(format, a...)
	inferer.Print(ast, "", msg)
}

// Debug2f prints debugging information about type inference.
func (inferer *Inferer) Debug2f(ast AST, format string, a ...interface{}) {
	if !inferer.scm.Params.Pragma.VerboseTypecheck ||
		!inferer.scm.Params.Verbose2() {
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

type InferTypes struct {
	Conclusive bool
	Types      []*types.Type
}

func (it *InferTypes) String() string {
	result := "{"

	for idx, t := range it.Types {
		if idx > 0 {
			result += ","
		}
		result += t.String()
	}
	if !it.Conclusive {
		result += "\u2026"
	}
	result += "}"

	return result
}

func (it *InferTypes) IsTypeVar() (*types.Type, bool) {
	if !it.Conclusive || len(it.Types) != 1 ||
		it.Types[0].Enum != types.EnumTypeVar {
		return nil, false
	}
	return it.Types[0], true
}

func (it *InferTypes) Type() *types.Type {
	if !it.Conclusive {
		return types.Unspecified
	}
	var result *types.Type
	for _, t := range it.Types {
		result = types.Unify(result, t)
	}
	if result == nil {
		return types.Unspecified
	}
	return result
}

// TypeFor unifies the InferTypes for the type variable tv. If any of
// the InferTypes is the type variable tv (recursive definition), it
// is removed from the unification.
func (it *InferTypes) TypeFor(tv *types.Type) *types.Type {
	if !it.Conclusive {
		return types.Unspecified
	}
	var result *types.Type
	for _, t := range it.Types {
		if tv.Enum == types.EnumTypeVar && t.IsA(tv) {
			continue
		}
		result = types.Unify(result, t)
	}
	if result == nil {
		return types.Unspecified
	}
	return result
}

func (it *InferTypes) LearnAll(ot *InferTypes) error {
	for _, t := range ot.Types {
		err := it.Learn(t)
		if err != nil {
			return err
		}
	}
	return nil
}

func (it *InferTypes) Learn(t *types.Type) error {
	// Do we already have something kind of t?
	for _, current := range it.Types {
		if current.IsKindOf(t) {
			return nil
		}
	}
	// Are we learning something more about an existing type?
	for idx, current := range it.Types {
		if t.IsKindOf(current) {
			it.Types[idx] = t
			return nil
		}
	}
	if it.Conclusive {
		_, ok := it.IsTypeVar()
		if !ok {
			// Can't learn more about a conclusive concrete type.
			return fmt.Errorf("can't learn more about %v with %v", it, t)
		}
		// The current conclusive type is a type variable. Add the new
		// type into the inferred types.
	}
	it.Types = append(it.Types, t)
	return nil
}

func (it *InferTypes) Add(t *types.Type) {
	for _, current := range it.Types {
		if t.IsA(current) {
			return
		}
	}
	it.Types = append(it.Types, t)
}

func (it *InferTypes) MatchAll(ot *InferTypes) error {
	for _, t := range ot.Types {
		if err := it.Match(t); err != nil {
			return err
		}
	}
	return nil
}

func (it *InferTypes) Match(t *types.Type) error {
	for _, current := range it.Types {
		if t.IsKindOf(current) {
			return nil
		}
	}
	return fmt.Errorf("can't match type %v with %v", it, t)
}

// Inferred stores type inference results.
type Inferred map[int]*InferTypes

func (inferred Inferred) String() string {
	if len(inferred) == 0 {
		return "\u2205"
	}

	var keys []int
	for k := range inferred {
		keys = append(keys, k)
	}
	sort.Ints(keys)

	result := "["
	first := true

	for _, k := range keys {
		if first {
			first = false
		} else {
			result += ","
		}
		result += fmt.Sprintf("%v=%v", k, inferred[k])
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

// CopyFacts creates a copy of the Inferred with definitive types
// copied.
func (inferred Inferred) CopyFacts() Inferred {
	if inferred == nil {
		return nil
	}
	result := make(Inferred)
	for k, v := range inferred {
		if v.Conclusive {
			result[k] = v
		}
	}
	return result
}

// Merge merges the learned types from the array of Inferred values
// into this Inferred.
func (inferred Inferred) Merge(learned []Inferred) error {
	keys := make(map[int]bool)

	for _, l := range learned {
		for k := range l {
			keys[k] = true
		}
	}
	for key := range keys {
		inferTypes := &InferTypes{
			Conclusive: true,
		}

		for _, l := range learned {
			v, ok := l[key]
			if !ok {
				inferTypes.Conclusive = false
			} else {
				for _, t := range v.Types {
					inferTypes.Add(t)
				}
			}
		}
		err := inferred.Learn(&types.Type{
			Enum:    types.EnumTypeVar,
			TypeVar: key,
		}, inferTypes)
		if err != nil {
			return err
		}
	}

	return nil
}

// Learn updates the type variable v's type with InferTypes it.
func (inferred Inferred) Learn(v *types.Type, it *InferTypes) error {
	if v.Enum != types.EnumTypeVar {
		learned := it.Type()
		if !learned.IsKindOf(v) {
			return fmt.Errorf("can't use %v as %v", learned, v)
		}
		return nil
	}
	old, ok := inferred[v.TypeVar]
	if !ok {
		inferred[v.TypeVar] = it
		return nil
	}
	if old.Conclusive {
		err := old.LearnAll(it)
		if err != nil {
			return fmt.Errorf("old.Conclusive: old=%v, it=%v: %v", old, it, err)
		}
		return nil
	}
	if it.Conclusive {
		err := it.MatchAll(old)
		if err != nil {
			return fmt.Errorf("it.Conclusive: %v", err)
		}
		return nil
	}
	for _, t := range it.Types {
		old.Add(t)
	}
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
			var result *types.Type
			var selfSkipped bool

			for _, ti := range mapped.Types {
				// Skip the type that is applied.
				if ti.IsA(t) {
					selfSkipped = true
					continue
				}
				result = types.Unify(result, ti)
			}
			if result == nil && selfSkipped {
				result = t
			}
			return result
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
func (env *InferEnv) Positive(branch *InferBranch) *InferEnv {
	result := env.Copy()
	if result.inferred == nil {
		result.inferred = result.inferer.inferred.CopyFacts()
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
func (env *InferEnv) Negative(branch *InferBranch) *InferEnv {
	result := env.Copy()
	if result.inferred == nil {
		result.inferred = result.inferer.inferred.CopyFacts()
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
	return t, true
}

func (env *InferEnv) resolve(ast AST) (*types.Type, bool) {
	_, _, err := ast.Infer(env)
	if err != nil {
		fmt.Printf("%s\n", err)
		return types.Unspecified, false
	}
	err = ast.Inferred(env)
	if err != nil {
		return types.Unspecified, false
	}
	return env.follow(ast.Type())
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
func (env *InferEnv) Learn(v *types.Type, it *InferTypes) error {
	if env.inferred != nil {
		return env.inferred.Learn(v, it)
	}
	return env.inferer.inferred.Learn(v, it)
}

// InferBranch defines the branch condition specific types.
type InferBranch struct {
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
	env.inferer.Debug2f(ast, " \u251c\u254c\u254c\u254c unify(%v,%v)\n",
		from, to)
	if from.Enum == types.EnumTypeVar {
		return unifyTypeVar(ast, env, from, to)
	}
	if to.Enum == types.EnumTypeVar {
		// The target type is unspecified i.e. accepting any types.
		// XXX if these came from function arguments, should implement
		// parametrized functions with from's type.
		return from, nil
	}
	if from.IsKindOf(to) {
		return from, nil
	}

	var t *types.Type
	var err error

	switch from.Enum {
	case types.EnumLambda:
		if to.Enum != from.Enum {
			return nil, errf(ast, "can't unify %v with %v", from, to)
		}
		if len(from.Args) < len(to.Args) {
			return nil, errf(ast, "too few arguments: got %v, need %v",
				len(from.Args), len(to.Args))
		}
		if len(from.Args) > len(to.Args) {
			return nil, errf(ast, "too many arguments: got %v, need %v",
				len(from.Args), len(to.Args))
		}
		result := &types.Type{
			Enum: types.EnumLambda,
		}
		for idx, arg := range from.Args {
			t, err = unify(ast, env, arg, to.Args[idx])
			if err != nil {
				return nil, errf(ast, "argument %d: %s", idx, err)
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
			return nil, errf(ast, "can't unify %v with %v", from, to)
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
			return nil, errf(ast, "can't unify %v with %v", from, to)
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
		return nil, errf(ast, "can't unify %v with %v", from, to)
	}
}

func unifyTypeVar(ast AST, env *InferEnv, tv, to *types.Type) (
	*types.Type, error) {

	if to == nil {
		panic("unifyTypeVar: to=nil")
	}
	if to.Enum == types.EnumAny {
		// We didn't learn anything.
		return tv, nil
	}
	err := env.Learn(tv, &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{to},
	})
	if err != nil {
		return nil, err
	}
	env.inferer.Debug2f(ast, " \u251c\u254c\u254c\u254c\u254c> %v=%v\n", tv, to)

	return to, nil
}

// Infer implements AST.Infer.
func (ast *ASTSequence) Infer(env *InferEnv) (
	*InferBranch, *InferTypes, error) {

	var err error
	for _, item := range ast.Items {
		_, ast.it, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		err = item.Inferred(env)
		if err != nil {
			return nil, nil, err
		}
		ast.t = item.Type()
	}

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTSequence) Inferred(env *InferEnv) error {
	for _, item := range ast.Items {
		err := item.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = env.Inferred().Apply(ast.t)

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTDefine) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	cached, ok := env.inferer.calls[ast]
	if ok {
		return nil, cached, nil
	}

	// Infer initializer type.
	var err error
	_, ast.it, err = ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	err = ast.Value.Inferred(env)
	if err != nil {
		return nil, nil, err
	}
	t := ast.Value.Type()

	// Check the current type of the name.
	sym := env.inferer.scm.Intern(ast.Name.Name)
	if !sym.GlobalType.IsA(types.Unspecified) {
		return nil, nil, errf(ast, "redefining symbol '%s'", ast.Name)
	}

	env.inferer.calls[ast] = ast.it

	env.inferer.Debugf(ast, "%v=%v\n", ast.Name.Name, t)
	sym.GlobalType = t
	ast.t = t

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTDefine) Inferred(env *InferEnv) error {
	err := ast.Value.Inferred(env)
	if err != nil {
		return err
	}
	ast.t = ast.Value.Type()
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTSet) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	// Infer initializer type.
	var err error
	_, ast.it, err = ast.Value.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	err = ast.Value.Inferred(env)
	if err != nil {
		return nil, nil, err
	}
	// XXX Should the type resolving and typechecks be moved to
	// Inferred()? Also in ASTDefine?
	t := ast.Value.Type()

	if ast.Binding != nil {
		// let-variables can be assigned with different value types.
		env.Set(ast.Name, t)
		ast.t = t
		return nil, ast.it, nil
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

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTSet) Inferred(env *InferEnv) error {
	err := ast.Value.Inferred(env)
	if err != nil {
		return err
	}
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTLet) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
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
		_, _, err = b.Init.Infer(initEnv)
		if err != nil {
			return nil, nil, err
		}
		err = b.Init.Inferred(initEnv)
		if err != nil {
			return nil, nil, err
		}
		t := b.Init.Type()

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
		_, ast.it, err = body.Infer(bodyEnv)
		if err != nil {
			return nil, nil, err
		}
	}
	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTLet) Inferred(env *InferEnv) error {
	for _, b := range ast.Bindings {
		err := b.Init.Inferred(env)
		if err != nil {
			return err
		}
	}
	for _, body := range ast.Body {
		err := body.Inferred(env)
		if err != nil {
			return err
		}
		ast.t = body.Type()
	}
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTIf) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	branch, _, err := ast.Cond.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	env.inferer.Debugf(ast, "If: branch=%v\n", branch)

	ast.it = &InferTypes{
		Conclusive: true,
	}

	trueEnv := env.Positive(branch)
	_, _, err = ast.True.Infer(trueEnv)
	if err != nil {
		return nil, nil, err
	}
	err = ast.True.Inferred(trueEnv)
	if err != nil {
		return nil, nil, err
	}
	ast.it.Add(ast.True.Type())

	if ast.False == nil {
		ast.it.Add(types.Boolean)
	} else {
		falseEnv := env.Negative(branch)
		_, _, err = ast.False.Infer(falseEnv)
		if err != nil {
			return nil, nil, err
		}
		err = ast.False.Inferred(falseEnv)
		if err != nil {
			return nil, nil, err
		}
		ast.it.Add(ast.False.Type())

		// The if expression is conclusive. Merge learnings into
		// global curriculum.
		env.inferer.Debugf(ast, "    true  : %v\n", trueEnv.inferred)
		env.inferer.Debugf(ast, "    false : %v\n", falseEnv.inferred)
		env.Inferred().Merge([]Inferred{trueEnv.inferred, falseEnv.inferred})
		env.inferer.Debugf(ast, " => merged: %v\n", env.Inferred())
	}

	env.inferer.Debugf(ast, "If: it=%v\n", ast.it)

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTIf) Inferred(env *InferEnv) error {
	err := ast.Cond.Inferred(env)
	if err != nil {
		return err
	}
	t := ast.Cond.Type()
	if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
		t.Concrete() && !t.IsA(types.Boolean) {
		env.inferer.Warningf(ast, "if test is not boolean: %v\n", t)
	}

	err = ast.True.Inferred(env)
	if err != nil {
		return err
	}
	if ast.False != nil {
		err = ast.False.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = ast.it.Type()

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTApply) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	// Resolve the type of the function.
	_, _, err := ast.Lambda.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	err = ast.Lambda.Inferred(env)
	if err != nil {
		return nil, nil, err
	}
	fnType := ast.Lambda.Type()

	env.inferer.Debugf(ast, "Apply: fnType=%v\n", fnType)
	if fnType.Concrete() && fnType.Enum != types.EnumLambda {
		return nil, nil, errf(ast, "invalid function: %v", fnType)
	}
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{fnType.Return},
	}
	ast.t = fnType.Return

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTApply) Inferred(env *InferEnv) error {
	err := ast.Lambda.Inferred(env)
	if err != nil {
		return err
	}
	err = ast.Args.Inferred(env)
	if err != nil {
		return err
	}
	ast.t = env.Inferred().Apply(ast.t)
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
func (ast *ASTCall) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env = env.Copy()

	// Resolve call argument types.
	env.inferer.Debugf(ast, "Call: argument types:\n")
	var argTypes []*types.Type
	for idx, arg := range ast.Args {
		_, it, err := arg.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		t := it.Type()

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
		_, _, err = ast.Func.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		err = ast.Func.Inferred(env)
		if err != nil {
			return nil, nil, err
		}
		fnType = ast.Func.Type()

		env.inferer.Debugf(ast, "Call: code: fnType=%v\n", fnType)
		switch fn := ast.Func.(type) {
		case *ASTIdentifier:
			fnName = fn.Name
		default:
			fnName = "lambda"
		}

		switch fnType.Enum {
		case types.EnumTypeVar, types.EnumUnspecified:
			env.inferer.Debugf(ast, "unspecified function %v => %v\n",
				fnType, types.Unspecified)
			return nil, &InferTypes{
				Conclusive: true,
				Types:      []*types.Type{types.Unspecified},
			}, nil

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
			// Check if the extra argument Kind are Optional or Rest.
			for i := len(ast.Args); i < al; i++ {
				if fnType.Args[i].Kind == types.Fixed {
					return nil, nil, errf(ast,
						"too few arguments got %v, need %v",
						len(ast.Args), al)
				}
			}
			fnType = fnType.Clone()
			fnType.Args = fnType.Args[:len(ast.Args)]
		}
	}

	retType, _, err := inferCall(ast, env, fnType, ast.Args)
	if err != nil {
		return nil, nil, errf(ast, "%s: %s", fnName, err)
	}
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{retType},
	}
	ast.t = retType

	// Check typecheck predicates.
	var branch *InferBranch
	id, ok := ast.Func.(*ASTIdentifier)
	if ok && len(fnType.Args) == 1 {
		t, ok := typePredicates[id.Name]
		if ok {
			argID, ok := ast.Args[0].(*ASTIdentifier)
			if ok {
				branch = &InferBranch{
					pos: env.inferer.NewEnv(),
				}
				env.inferer.Debugf(ast, "%s: %v=%v\n", id.Name, argID, t)
				branch.pos.bindings[argID.Name] = InferEnvBinding{
					t: t,
				}
			}
		}
	}

	return branch, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCall) Inferred(env *InferEnv) error {
	var err error
	if ast.Func != nil {
		err = ast.Func.Inferred(env)
		if err != nil {
			return err
		}
	}
	for _, arg := range ast.Args {
		err = arg.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = env.Inferred().Apply(ast.t)

	return nil
}

func inferCall(ast AST, env *InferEnv, fnType *types.Type, args []AST) (
	*types.Type, *types.Type, error) {

	// Create a new type variable for the return type of the function.
	rv := env.inferer.newTypeVar()

	env.inferer.Debugf(ast, "Call: fnType=%v\n", fnType)

	// Create a function type for the called function.
	callType := &types.Type{
		Enum:   types.EnumLambda,
		Args:   env.argTypes,
		Return: rv,
	}
	env.inferer.Debugf(ast, "Call: calltype=%v, unify:\n", callType)
	env.inferer.Debugf(ast, " \u256d\u2574 %v\n", callType)
	env.inferer.Debugf(ast, " \u251c\u2574 %v\n", fnType)

	// Unify fnTypeSubst with callType.
	unified, err := Unify(ast, env, callType, fnType)
	if err != nil {
		env.inferer.Warningf(ast, "Unify: %s:\n", err)
		env.inferer.Warningf(ast, " \u251c\u2574\u22a5 %v\n", callType)
		env.inferer.Warningf(ast, " \u251c\u2574\u22a5 %v\n", fnType)
		env.inferer.Warningf(ast, " \u2514\u2574\u22a5 %v\n", env.Inferred())
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
		result := &types.Type{
			Enum:   types.EnumLambda,
			Return: types.Number,
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
func (ast *ASTCallUnary) Infer(env *InferEnv) (
	*InferBranch, *InferTypes, error) {

	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env.inferer.Debugf(ast, "CallUnary: %v\n", ast.Op)

	// Resolve argument type.
	_, _, err := ast.Arg.Infer(env)
	if err != nil {
		return nil, nil, err
	}
	err = ast.Arg.Inferred(env)
	if err != nil {
		return nil, nil, err
	}
	argType := ast.Arg.Type()
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

	retType, _, err := inferCall(ast, env, fnType, []AST{
		ast.Arg,
	})
	if err != nil {
		return nil, nil, err
	}
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{retType},
	}
	ast.t = retType

	// Check pair? predicate.
	var branch *InferBranch
	if ast.Op == OpPairp {
		id, ok := ast.Arg.(*ASTIdentifier)
		if ok {
			branch = &InferBranch{
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

	return branch, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCallUnary) Inferred(env *InferEnv) error {
	err := ast.Arg.Inferred(env)
	if err != nil {
		return err
	}
	ast.t = env.Inferred().Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTLambda) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	env.inferer.scm.Params.PushScope()
	defer env.inferer.scm.Params.PopScope()

	cached, ok := env.inferer.calls[ast]
	if ok {
		return nil, cached, nil
	}
	lambdaType := &types.Type{
		Enum:   types.EnumLambda,
		Return: env.inferer.newTypeVar(),
	}
	ast.t = lambdaType
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{lambdaType},
	}
	env.inferer.calls[ast] = ast.it

	env = env.Copy()

	var args []*types.Type
	for idx, arg := range ast.Args.Fixed {
		t := env.inferer.newTypeVar()

		env.inferer.Debugf(ast, "\u03bb: arg[%v]=%v\n", idx, t)

		args = append(args, t)
		env.Set(arg.Name, t)

		lambdaType.Args = append(lambdaType.Args, t)
	}
	if ast.Args.Rest != nil {
		t := &types.Type{
			Enum: types.EnumPair,
			Car:  env.inferer.newTypeVar(),
			Cdr:  env.inferer.newTypeVar(),
		}
		args = append(args, t)
		env.Set(ast.Args.Rest.Name, t)

		lambdaType.Rest = t
	}
	env.inferer.Debugf(ast, "\u03bb: env=%v\n", env)

	var it *InferTypes
	var err error

	for _, item := range ast.Body {
		_, it, err = item.Infer(env)
		if err != nil {
			return nil, nil, err
		}
	}
	t := it.TypeFor(lambdaType.Return)
	env.inferer.Debugf(ast, "\u03bb: return  : %v=%v=>%v\n",
		lambdaType.Return, it, t)

	if t == nil {
		t = types.Unspecified
	}
	// XXX this can be number=number
	err = env.Learn(lambdaType.Return, &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{t},
	})
	if err != nil {
		return nil, nil, err
	}

	env.inferer.Debugf(ast, "\u03bb: type    : %v\n", lambdaType)
	env.inferer.Debugf(ast, "\u03bb: inferred: %v\n", env.inferer.inferred)

	ast.Inferred(env)

	if ast.Define {
		sym := env.inferer.scm.Intern(ast.Name.Name)
		if !sym.GlobalType.IsA(types.Unspecified) {
			return nil, nil, errf(ast, "redefining symbol '%s'", ast.Name)
		}
		sym.GlobalType = ast.t
		env.inferer.Debugf(ast, "%v=%v\n", ast.Name, ast.t)
	}

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTLambda) Inferred(env *InferEnv) error {
	for _, item := range ast.Body {
		err := item.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = env.Inferred().Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTConstant) Infer(env *InferEnv) (
	*InferBranch, *InferTypes, error) {

	var t *types.Type
	if ast.Value == nil {
		t = types.Nil
	} else {
		t = ast.Value.Type()
	}
	return nil, &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{t},
	}, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTConstant) Inferred(env *InferEnv) error {
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTIdentifier) Infer(env *InferEnv) (
	*InferBranch, *InferTypes, error) {

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
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{ast.t},
	}

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTIdentifier) Inferred(env *InferEnv) error {
	ast.t = env.Inferred().Apply(ast.t)
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTCond) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var err error
	var branches []Inferred

	ast.it = &InferTypes{
		Conclusive: ast.Conclusive,
	}
	if !ast.Conclusive {
		ast.it.Add(types.Boolean)
	}

	for _, choice := range ast.Choices {
		var choiceBranch *InferBranch
		var choiceType *types.Type

		if choice.Cond == nil {
			choiceType = types.Boolean
		} else {
			choiceBranch, _, err = choice.Cond.Infer(env)
			if err != nil {
				return nil, nil, err
			}
			err = choice.Cond.Inferred(env)
			if err != nil {
				return nil, nil, err
			}
			choiceType = choice.Cond.Type()

			if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
				choice.Func == nil && choiceType.Concrete() &&
				!choiceType.IsA(types.Boolean) {
				choice.Cond.Locator().From().
					Warningf("choice is not boolean: %v\n", choiceType)
			}
		}

		if choice.Func != nil {
			_, _, err := choice.Func.Infer(env)
			if err != nil {
				return nil, nil, err
			}
			err = choice.Func.Inferred(env)
			if err != nil {
				return nil, nil, err
			}
			fnType := choice.Func.Type()

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
			ast.it.Add(fnType.Return)
		} else if len(choice.Exprs) == 0 {
			ast.it.Add(choiceType)
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
				err = expr.Inferred(choiceEnv)
				if err != nil {
					return nil, nil, err
				}
				choiceType = expr.Type()
			}
			ast.it.Add(choiceType)
			env.inferer.Debugf(ast, "cond types=%v\n", ast.it)

			if ast.Conclusive {
				branches = append(branches, choiceEnv.inferred)
			}
		}
	}

	if ast.Conclusive {
		// The cond expression is conclusive. Merge learnings into
		// global curriculum.
		for idx, branch := range branches {
			env.inferer.Debugf(ast, "cond: %v : %v\n", idx, branch)
		}
		env.Inferred().Merge(branches)
		env.inferer.Debugf(ast, "cond: => : %v\n", env.Inferred())
	}

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCond) Inferred(env *InferEnv) error {
	for _, choice := range ast.Choices {
		if choice.Cond != nil {
			err := choice.Cond.Inferred(env)
			if err != nil {
				return err
			}
		}
		if choice.Func != nil {
			err := choice.Func.Inferred(env)
			if err != nil {
				return nil
			}
		}
		for _, expr := range choice.Exprs {
			err := expr.Inferred(env)
			if err != nil {
				return err
			}
		}
	}
	ast.t = ast.it.Type()
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTCase) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	var branches []Inferred

	result := &InferTypes{
		Conclusive: ast.Conclusive,
	}
	if !ast.Conclusive {
		result.Add(types.Boolean)
	}

	_, _, err := ast.Expr.Infer(env)
	if err != nil {
		return nil, nil, err
	}

	for _, choice := range ast.Choices {
		choiceEnv := env.Positive(nil)
		var choiceType *types.Type

		for _, expr := range choice.Exprs {
			_, _, err = expr.Infer(choiceEnv)
			if err != nil {
				return nil, nil, err
			}
		}
		for _, expr := range choice.Exprs {
			err = expr.Inferred(choiceEnv)
			if err != nil {
				return nil, nil, err
			}
			choiceType = expr.Type()
		}
		result.Add(choiceType)
		env.inferer.Debugf(ast, "case types=%v\n", result)

		if ast.Conclusive {
			branches = append(branches, choiceEnv.inferred)
		}
	}

	if ast.Conclusive {
		// The case expression is conclusive. Merge learnings into
		// global curriculum.
		for idx, branch := range branches {
			env.inferer.Debugf(ast, "case: %v : %v\n", idx, branch)
		}
		env.Inferred().Merge(branches)
		env.inferer.Debugf(ast, "case: => : %v\n", env.Inferred())
	}

	ast.it = result

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTCase) Inferred(env *InferEnv) error {
	err := ast.Expr.Inferred(env)
	if err != nil {
		return err
	}
	for _, choice := range ast.Choices {
		for _, expr := range choice.Exprs {
			err = expr.Inferred(env)
			if err != nil {
				return nil
			}
		}
	}
	ast.t = ast.it.Type()
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTAnd) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	result := &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{types.Boolean},
	}

	for _, expr := range ast.Exprs {
		_, _, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		err = expr.Inferred(env)
		if err != nil {
			return nil, nil, err
		}
		t := expr.Type()
		if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
			t.Concrete() && !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("and expr is not boolean: %v\n", t)
		}
		result.Add(t)
	}
	ast.it = result

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTAnd) Inferred(env *InferEnv) error {
	for _, expr := range ast.Exprs {
		err := expr.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = ast.it.Type()
	return nil
}

// Infer implements AST.Infer.
func (ast *ASTOr) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
	env.inferer.Enter(ast)
	defer env.inferer.Exit(ast)

	result := &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{types.Boolean},
	}

	for _, expr := range ast.Exprs {
		_, _, err := expr.Infer(env)
		if err != nil {
			return nil, nil, err
		}
		err = expr.Inferred(env)
		if err != nil {
			return nil, nil, err
		}
		t := expr.Type()
		if !env.inferer.scm.Params.Pragma.NoCheckBooleanExprs &&
			t.Concrete() && !t.IsA(types.Boolean) {
			expr.Locator().From().Warningf("or expr is not boolean: %v\n", t)
		}
		result.Add(t)
	}
	ast.it = result

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTOr) Inferred(env *InferEnv) error {
	for _, expr := range ast.Exprs {
		err := expr.Inferred(env)
		if err != nil {
			return err
		}
	}
	ast.t = ast.it.Type()

	return nil
}

// Infer implements AST.Infer.
func (ast *ASTPragma) Infer(env *InferEnv) (*InferBranch, *InferTypes, error) {
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
	ast.t = types.Unspecified
	ast.it = &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{ast.t},
	}

	return nil, ast.it, nil
}

// Inferred implements AST.Inferred.
func (ast *ASTPragma) Inferred(env *InferEnv) error {
	return nil
}

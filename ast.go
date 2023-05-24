//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

// AST defines an abstract syntax tree entry
type AST interface {
	Locator() Locator
	Equal(o AST) bool
	Type() *types.Type
	Typecheck(lib *Library, round int) error
	Bytecode(lib *Library) error
}

var (
	_ AST = &ASTSequence{}
	_ AST = &ASTDefine{}
	_ AST = &ASTSet{}
	_ AST = &ASTLet{}
	_ AST = &ASTIf{}
	_ AST = &ASTApply{}
	_ AST = &ASTCall{}
	_ AST = &ASTCallUnary{}
	_ AST = &ASTLambda{}
	_ AST = &ASTConstant{}
	_ AST = &ASTIdentifier{}
	_ AST = &ASTCond{}
	_ AST = &ASTCase{}
	_ AST = &ASTAnd{}
	_ AST = &ASTOr{}
)

// ASTSequence implements a (begin ...) sequence.
type ASTSequence struct {
	From  Locator
	Items []AST
}

// Add adds an item to the sequence.
func (ast *ASTSequence) Add(item AST) {
	ast.Items = append(ast.Items, item)
}

// Locator implements AST.Locator.
func (ast *ASTSequence) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTSequence) Equal(o AST) bool {
	oast, ok := o.(*ASTSequence)
	if !ok {
		return false
	}
	if len(ast.Items) != len(oast.Items) {
		return false
	}
	for idx, item := range ast.Items {
		if !item.Equal(oast.Items[idx]) {
			return false
		}
	}
	return true
}

// Type implements AST.Type.
func (ast *ASTSequence) Type() *types.Type {
	if len(ast.Items) == 0 {
		return types.Unspecified
	}
	return ast.Items[len(ast.Items)-1].Type()
}

// Typecheck implements AST.Type.
func (ast *ASTSequence) Typecheck(lib *Library, round int) error {
	for _, item := range ast.Items {
		err := item.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSequence) Bytecode(lib *Library) error {
	for _, item := range ast.Items {
		err := item.Bytecode(lib)
		if err != nil {
			return err
		}
	}
	return nil
}

// ASTDefine implements (define name value).
type ASTDefine struct {
	From  Locator
	Name  *Identifier
	Flags Flags
	Value AST
}

// Locator implements AST.Locator.
func (ast *ASTDefine) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTDefine) Equal(o AST) bool {
	oast, ok := o.(*ASTDefine)
	if !ok {
		return false
	}
	return ast.Name.Name == oast.Name.Name &&
		ast.Flags == oast.Flags &&
		ast.Value.Equal(oast.Value)
}

// Type implements AST.Type.
func (ast *ASTDefine) Type() *types.Type {
	return ast.Value.Type()
}

// Typecheck implements AST.Type.
func (ast *ASTDefine) Typecheck(lib *Library, round int) error {
	err := ast.Value.Typecheck(lib, round)
	if err != nil {
		return err
	}

	sym := lib.scm.Intern(ast.Name.Name)
	nt := ast.Value.Type()

	if round == 0 {
		if !sym.GlobalType.IsA(types.Unspecified) {
			return ast.From.Errorf("redefining symbol '%s'", ast.Name.Name)
		}
		sym.GlobalType = nt
		lib.recheck = true
	} else {
		if !nt.IsA(sym.GlobalType) {
			sym.GlobalType = nt
			lib.recheck = true
		}
	}

	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTDefine) Bytecode(lib *Library) error {
	err := ast.Value.Bytecode(lib)
	if err != nil {
		return err
	}
	return lib.define(ast.From, ast.Name, ast.Flags)
}

// ASTSet implements (set name value).
type ASTSet struct {
	From    Locator
	Name    string
	Binding *EnvBinding
	Value   AST
}

// Locator implements AST.Locator.
func (ast *ASTSet) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTSet) Equal(o AST) bool {
	oast, ok := o.(*ASTSet)
	if !ok {
		return false
	}
	if ast.Binding == nil && oast.Binding != nil {
		return false
	}
	if ast.Binding != nil && oast.Binding == nil {
		return false
	}
	return ast.Name == oast.Name && ast.Value.Equal(oast.Value)
}

// Type implements AST.Type.
func (ast *ASTSet) Type() *types.Type {
	return ast.Value.Type()
}

// Typecheck implements AST.Type.
func (ast *ASTSet) Typecheck(lib *Library, round int) error {
	vtype := ast.Value.Type()

	if ast.Binding != nil {
		// let-variables can be assigned with different value types.
		return nil
	}
	sym := lib.scm.Intern(ast.Name)
	if sym.GlobalType.IsA(types.Unspecified) {
		return ast.From.Errorf("setting undefined symbol '%s'", ast.Name)
	}
	if !vtype.IsKindOf(sym.GlobalType) {
		return ast.From.Errorf("can't assign %s to variable of type %s",
			vtype, sym.GlobalType)
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSet) Bytecode(lib *Library) error {
	err := ast.Value.Bytecode(lib)
	if err != nil {
		return err
	}

	if ast.Binding != nil {
		if ast.Binding.Frame.Type == TypeStack {
			lib.addInstr(ast.From, OpLocalSet, nil,
				ast.Binding.Frame.Index+ast.Binding.Index)
		} else {
			instr := lib.addInstr(ast.From, OpEnvSet, nil,
				ast.Binding.Frame.Index)
			instr.J = ast.Binding.Index
		}
	} else {
		instr := lib.addInstr(ast.From, OpGlobalSet, nil, 0)
		instr.Sym = lib.scm.Intern(ast.Name)
	}

	return nil
}

// ASTLet implements let syntaxes.
type ASTLet struct {
	From     Locator
	Kind     Keyword
	Captures bool
	Tail     bool
	Bindings []*ASTLetBinding
	Body     []AST
}

// ASTLetBinding implements a let binding.
type ASTLetBinding struct {
	From    Locator
	Binding *EnvBinding
	Init    AST
}

// Locator implements AST.Locator.
func (ast *ASTLet) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTLet) Equal(o AST) bool {
	oast, ok := o.(*ASTLet)
	if !ok {
		return false
	}
	if ast.Kind != oast.Kind || ast.Captures != oast.Captures ||
		ast.Tail != oast.Tail || len(ast.Bindings) != len(oast.Bindings) ||
		len(ast.Body) != len(oast.Body) {
		return false
	}
	for idx, b := range ast.Bindings {
		if b.Binding.Index != oast.Bindings[idx].Binding.Index ||
			b.Binding.Frame.Index != oast.Bindings[idx].Binding.Frame.Index {
			return false
		}
	}
	for idx, item := range ast.Body {
		if !item.Equal(oast.Body[idx]) {
			return false
		}
	}
	return true
}

// Type implements AST.Type.
func (ast *ASTLet) Type() *types.Type {
	return ast.Body[len(ast.Body)-1].Type()
}

// Typecheck implements AST.Type.
func (ast *ASTLet) Typecheck(lib *Library, round int) error {
	for _, b := range ast.Bindings {
		err := b.Init.Typecheck(lib, round)
		if err != nil {
			return err
		}
		nt := b.Init.Type()
		if round == 0 {
			b.Binding.Type = nt
			if ast.Kind == KwLetrec {
				lib.recheck = true
			}
		} else {
			if !nt.IsKindOf(b.Binding.Type) {
				b.Binding.Type = nt
				lib.recheck = true
			}
		}
	}
	for _, body := range ast.Body {
		err := body.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLet) Bytecode(lib *Library) error {
	lib.addPushS(ast.From, len(ast.Bindings), ast.Captures)

	for _, binding := range ast.Bindings {
		err := binding.Init.Bytecode(lib)
		if err != nil {
			return err
		}
		if binding.Binding.Frame.Type == TypeStack {
			lib.addInstr(binding.From, OpLocalSet, nil,
				binding.Binding.Frame.Index+binding.Binding.Index)
		} else {
			instr := lib.addInstr(binding.From, OpEnvSet, nil,
				binding.Binding.Frame.Index)
			instr.J = binding.Binding.Index
		}
	}

	for _, item := range ast.Body {
		err := item.Bytecode(lib)
		if err != nil {
			return err
		}
	}
	if !ast.Tail {
		lib.addPopS(nil, len(ast.Bindings), ast.Captures)
	}

	return nil
}

// ASTIf implements if syntax.
type ASTIf struct {
	From  Locator
	Cond  AST
	True  AST
	False AST
}

// Locator implements AST.Locator.
func (ast *ASTIf) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTIf) Equal(o AST) bool {
	oast, ok := o.(*ASTIf)
	if !ok {
		return false
	}
	if !ast.Cond.Equal(oast.Cond) {
		return false
	}
	if !ast.True.Equal(oast.True) {
		return false
	}
	if ast.False == nil {
		if oast.False != nil {
			return false
		}
	} else if oast.False == nil {
		return false
	} else if !ast.False.Equal(oast.False) {
		return false
	}

	return true
}

// Type implements AST.Type.
func (ast *ASTIf) Type() *types.Type {
	if ast.False == nil {
		return types.Unify(ast.Cond.Type(), ast.True.Type())
	}
	return types.Unify(ast.True.Type(), ast.False.Type())
}

// Typecheck implements AST.Type.
func (ast *ASTIf) Typecheck(lib *Library, round int) error {
	err := ast.Cond.Typecheck(lib, round)
	if err != nil {
		return err
	}
	err = ast.True.Typecheck(lib, round)
	if err != nil {
		return err
	}
	if ast.False != nil {
		err = ast.False.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTIf) Bytecode(lib *Library) error {
	labelFalse := lib.newLabel()
	labelEnd := lib.newLabel()

	err := ast.Cond.Bytecode(lib)
	if err != nil {
		return err
	}

	if ast.False == nil {
		// (if cond t)
		instr := lib.addInstr(ast.From, OpIfNot, nil, 0)
		instr.J = labelEnd.I

		err = ast.True.Bytecode(lib)
		if err != nil {
			return err
		}
	} else {
		// (if cond t f)
		instr := lib.addInstr(ast.From, OpIfNot, nil, 0)
		instr.J = labelFalse.I

		err = ast.True.Bytecode(lib)
		if err != nil {
			return err
		}
		instr = lib.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I

		lib.addLabel(labelFalse)
		err = ast.False.Bytecode(lib)
		if err != nil {
			return err
		}
	}
	lib.addLabel(labelEnd)

	return nil
}

// ASTApply implements apply syntax.
type ASTApply struct {
	From   Locator
	Lambda AST
	Args   AST
	Tail   bool
}

// Locator implements AST.Locator.
func (ast *ASTApply) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTApply) Equal(o AST) bool {
	oast, ok := o.(*ASTApply)
	if !ok {
		return false
	}
	return ast.Lambda.Equal(oast.Lambda) &&
		ast.Args.Equal(oast.Args) &&
		ast.Tail == oast.Tail
}

// Type implements AST.Type.
func (ast *ASTApply) Type() *types.Type {
	t := ast.Lambda.Type()
	if t.Enum != types.EnumLambda {
		return types.Unspecified
	}
	return t.Return
}

// Typecheck implements AST.Type.
func (ast *ASTApply) Typecheck(lib *Library, round int) error {
	// XXX
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTApply) Bytecode(lib *Library) error {
	err := ast.Lambda.Bytecode(lib)
	if err != nil {
		return err
	}

	// Create a call frame.
	lib.addInstr(nil, OpPushF, nil, 0)

	// Compile arguments.
	err = ast.Args.Bytecode(lib)
	if err != nil {
		return err
	}

	// Push apply scope.
	lib.addInstr(nil, OpPushA, nil, 0)

	lib.addCall(nil, -1, ast.Tail)

	return nil
}

// ASTCall implements function call syntax.
type ASTCall struct {
	From     Locator
	Inline   bool
	InlineOp Operand
	Func     AST
	ArgFrame *EnvFrame
	Args     []AST
	ArgLocs  []Locator
	Tail     bool
}

// Locator implements AST.Locator.
func (ast *ASTCall) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTCall) Equal(o AST) bool {
	oast, ok := o.(*ASTCall)
	if !ok {
		return false
	}
	if !ast.Func.Equal(oast.Func) {
		return false
	}
	if len(ast.Args) != len(oast.Args) {
		return false
	}
	for idx, arg := range ast.Args {
		if !arg.Equal(oast.Args[idx]) {
			return false
		}
	}
	return ast.Tail == oast.Tail
}

var inlineCallTypes = map[Operand]*types.Type{
	OpCons: {
		Enum: types.EnumPair,
		Car:  types.Unspecified,
		Cdr:  types.Unspecified,
	},
	OpAdd: types.Number,
	OpSub: types.Number,
	OpEq:  types.Boolean,
	OpLt:  types.Boolean,
	OpGt:  types.Boolean,
	OpLe:  types.Boolean,
	OpGe:  types.Boolean,
}

// Type implements AST.Type.
func (ast *ASTCall) Type() *types.Type {
	if ast.Inline {
		t, ok := inlineCallTypes[ast.InlineOp]
		if !ok {
			panic(fmt.Sprintf("unknown inline operand: %v", ast.InlineOp))
		}
		return t
	}
	t := ast.Func.Type()
	if t.Enum != types.EnumLambda {
		return types.Unspecified
	}
	return t.Return
}

// Typecheck implements AST.Type.
func (ast *ASTCall) Typecheck(lib *Library, round int) error {
	for _, arg := range ast.Args {
		err := arg.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	// XXX inline call
	if !ast.Inline {
		err := ast.Func.Typecheck(lib, round)
		if err != nil {
			return err
		}
		ft := ast.Func.Type()
		if ft.IsA(types.Unspecified) || ft.IsA(types.Any) {
			return nil
		}
		if ft.Enum != types.EnumLambda {
			return ast.Func.Locator().Errorf("invalid procedure: %s", ft)
		}
		if len(ast.Args) < ft.MinArgs() {
			return ast.From.Errorf("too few arguments: got %v, need %v",
				len(ast.Args), ft.MinArgs())
		}
		if len(ast.Args) > ft.MaxArgs() {
			return ast.From.Errorf("too many arguments: got %v, max %v",
				len(ast.Args), ft.MinArgs())
		}
		// Check argument types.
		for idx, arg := range ast.Args {
			at := arg.Type()
			if idx < len(ft.Args) {
				if !at.IsKindOf(ft.Args[idx]) {
					return arg.Locator().Errorf("invalid argument %v, expected %v",
						at, ft.Args[idx])
				}
			}
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCall) Bytecode(lib *Library) error {
	if !ast.Inline {
		err := ast.Func.Bytecode(lib)
		if err != nil {
			return nil
		}
		// Create call frame.
		lib.addInstr(ast.From, OpPushF, nil, 0)
	}

	// Push argument scope.
	lib.addInstr(ast.From, OpPushS, nil, len(ast.Args))

	// Evaluate arguments.
	for idx, arg := range ast.Args {
		err := arg.Bytecode(lib)
		if err != nil {
			return err
		}
		lib.addInstr(ast.ArgLocs[idx], OpLocalSet, nil, ast.ArgFrame.Index+idx)
	}

	if ast.Inline {
		lib.addInstr(ast.From, ast.InlineOp, nil, 0)
		lib.addInstr(ast.From, OpPopS, nil, len(ast.Args))
	} else {
		lib.addCall(nil, len(ast.Args), ast.Tail)
	}

	return nil
}

// ASTCallUnary implements inlined unary function calls.
type ASTCallUnary struct {
	From Locator
	Op   Operand
	Arg  AST
}

// Locator implements AST.Locator.
func (ast *ASTCallUnary) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTCallUnary) Equal(o AST) bool {
	oast, ok := o.(*ASTCallUnary)
	if !ok {
		return false
	}
	return ast.Op == oast.Op && ast.Arg.Equal(oast.Arg)
}

var inlineUnaryTypes = map[Operand]*types.Type{
	OpPairp: types.Boolean,
	OpCar:   types.Unspecified,
	OpCdr:   types.Unspecified,
	OpNullp: types.Boolean,
	OpZerop: types.Boolean,
	OpNot:   types.Boolean,
}

// Type implements AST.Type.
func (ast *ASTCallUnary) Type() *types.Type {
	t, ok := inlineUnaryTypes[ast.Op]
	if !ok {
		panic(fmt.Sprintf("unknown inline unary operand: %v", ast.Op))
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTCallUnary) Typecheck(lib *Library, round int) error {
	// XXX
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCallUnary) Bytecode(lib *Library) error {
	err := ast.Arg.Bytecode(lib)
	if err != nil {
		return err
	}
	lib.addInstr(ast.From, ast.Op, nil, 0)

	return nil
}

// ASTLambda implements lambda syntax.
type ASTLambda struct {
	From     Locator
	Name     *Identifier
	Args     Args
	Body     []AST
	Env      *Env
	Captures bool
	Define   bool
	Flags    Flags
}

// Locator implements AST.Locator.
func (ast *ASTLambda) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTLambda) Equal(o AST) bool {
	oast, ok := o.(*ASTLambda)
	if !ok {
		return false
	}
	if !ast.Args.Equal(oast.Args) {
		return false
	}
	if len(ast.Body) != len(oast.Body) {
		return false
	}
	for idx, body := range ast.Body {
		if !body.Equal(oast.Body[idx]) {
			return false
		}
	}
	return ast.Captures == oast.Captures && ast.Flags == oast.Flags
}

// Type implements AST.Type.
func (ast *ASTLambda) Type() *types.Type {
	t := &types.Type{
		Enum:   types.EnumLambda,
		Return: ast.Body[len(ast.Body)-1].Type(),
	}
	for _, arg := range ast.Args.Fixed {
		t.Args = append(t.Args, arg.Type)
	}
	if ast.Args.Rest != nil {
		t.Rest = ast.Args.Rest.Type
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTLambda) Typecheck(lib *Library, round int) error {
	for _, body := range ast.Body {
		err := body.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	if ast.Name == nil {
		return nil
	}

	sym := lib.scm.Intern(ast.Name.Name)
	nt := ast.Type()

	if round == 0 {
		if !sym.GlobalType.IsA(types.Unspecified) {
			return ast.From.Errorf("redefining symbol '%s'", ast.Name.Name)
		}
		sym.GlobalType = nt
		lib.recheck = true
	} else {
		if !nt.IsA(sym.GlobalType) {
			sym.GlobalType = nt
			lib.recheck = true
		}
	}

	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLambda) Bytecode(lib *Library) error {
	lib.addInstr(ast.From, OpLambda, nil, len(lib.lambdas))
	lib.lambdas = append(lib.lambdas, &lambdaCompilation{
		Name:     ast.Name,
		Args:     ast.Args,
		Body:     ast.Body,
		Env:      ast.Env,
		Captures: ast.Captures,
	})
	if ast.Define {
		err := lib.define(ast.From, ast.Name, ast.Flags)
		if err != nil {
			return err
		}
	}

	return nil
}

// ASTConstant implements contant values.
type ASTConstant struct {
	From  Locator
	Value Value
}

// Locator implements AST.Locator.
func (ast *ASTConstant) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTConstant) Equal(o AST) bool {
	oast, ok := o.(*ASTConstant)
	if !ok {
		return false
	}
	return ast.Value.Equal(oast.Value)
}

// Type implements AST.Type.
func (ast *ASTConstant) Type() *types.Type {
	if ast.Value == nil {
		return types.Nil
	}
	return ast.Value.Type()
}

// Typecheck implements AST.Type.
func (ast *ASTConstant) Typecheck(lib *Library, round int) error {
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTConstant) Bytecode(lib *Library) error {
	lib.addInstr(nil, OpConst, ast.Value, 0)
	return nil
}

// ASTIdentifier implements identifer references.
type ASTIdentifier struct {
	From    Locator
	Name    string
	Binding *EnvBinding
	Global  *Identifier
}

// Locator implements AST.Locator.
func (ast *ASTIdentifier) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTIdentifier) Equal(o AST) bool {
	oast, ok := o.(*ASTIdentifier)
	if !ok {
		return false
	}
	if ast.Name != oast.Name {
		return false
	}
	if ast.Binding == nil && oast.Binding != nil {
		return false
	}
	if ast.Binding != nil && oast.Binding == nil {
		return false
	}
	return true
}

// Type implements AST.Type.
func (ast *ASTIdentifier) Type() *types.Type {
	if ast.Binding != nil {
		return ast.Binding.Type
	}
	return ast.Global.GlobalType
}

// Typecheck implements AST.Type.
func (ast *ASTIdentifier) Typecheck(lib *Library, round int) error {
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTIdentifier) Bytecode(lib *Library) error {
	if ast.Binding != nil {
		if ast.Binding.Frame.Type == TypeStack {
			lib.addInstr(ast.From, OpLocal, nil,
				ast.Binding.Frame.Index+ast.Binding.Index)
		} else {
			instr := lib.addInstr(ast.From, OpEnv, nil, ast.Binding.Frame.Index)
			instr.J = ast.Binding.Index
		}
	} else {
		instr := lib.addInstr(ast.From, OpGlobal, nil, 0)
		instr.Sym = lib.scm.Intern(ast.Name)
	}
	return nil
}

// ASTCond implements cond syntax.
type ASTCond struct {
	From     Locator
	Choices  []*ASTCondChoice
	Tail     bool
	Captures bool
}

// ASTCondChoice implements a cond choice.
type ASTCondChoice struct {
	From           Locator
	Cond           AST
	Func           AST
	FuncValueFrame *EnvFrame
	FuncArgsFrame  *EnvFrame
	Exprs          []AST
}

// Equal test if this choice is equal to the argument choice.
func (c *ASTCondChoice) Equal(o *ASTCondChoice) bool {
	if len(c.Exprs) != len(o.Exprs) {
		return false
	}
	for idx, expr := range c.Exprs {
		if !expr.Equal(o.Exprs[idx]) {
			return false
		}
	}

	return c.Cond.Equal(o.Cond) && c.Func.Equal(o.Func)
}

// Locator implements AST.Locator.
func (ast *ASTCond) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTCond) Equal(o AST) bool {
	oast, ok := o.(*ASTCond)
	if !ok {
		return false
	}
	if len(ast.Choices) != len(oast.Choices) {
		return false
	}
	for idx, choice := range ast.Choices {
		if !choice.Equal(oast.Choices[idx]) {
			return false
		}
	}
	return ast.Tail == oast.Tail && ast.Captures == oast.Captures
}

// Type implements AST.Type.
func (ast *ASTCond) Type() *types.Type {
	var t *types.Type
	var hadDefault bool

	for _, choice := range ast.Choices {
		if choice.Cond == nil {
			hadDefault = true
		}
		if len(choice.Exprs) == 0 {
			if hadDefault {
				return types.Boolean
			}
			return choice.Cond.Type()
		}
		t = types.Unify(t, choice.Exprs[len(choice.Exprs)-1].Type())
	}
	if !hadDefault {
		// No default so value of the last cond (false) is one valid
		// return value.
		t = types.Unify(t, types.Boolean)
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTCond) Typecheck(lib *Library, round int) error {
	for _, choice := range ast.Choices {
		if choice.Cond != nil {
			err := choice.Cond.Typecheck(lib, round)
			if err != nil {
				return err
			}
		}
		if choice.Func != nil {
			err := choice.Func.Typecheck(lib, round)
			if err != nil {
				return err
			}
		}
		for _, expr := range choice.Exprs {
			err := expr.Typecheck(lib, round)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCond) Bytecode(lib *Library) error {
	labelEnd := lib.newLabel()

	var labelClause *Instr

	for i, choice := range ast.Choices {
		if labelClause != nil {
			lib.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(ast.Choices) {
			next = lib.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		// The choice.Cond is nil for else case.
		if choice.Cond != nil {
			// Compile condition.
			err := choice.Cond.Bytecode(lib)
			if err != nil {
				return err
			}
			instr := lib.addInstr(choice.From, OpIfNot, nil, 0)
			instr.J = next.I
		}
		// cond => func
		if choice.Func != nil {
			// Push value scope.
			lib.addInstr(choice.From, OpPushS, nil, 1)

			// Save value
			lib.addInstr(choice.From, OpLocalSet, nil,
				choice.FuncValueFrame.Index)

			// Compile function.
			err := choice.Func.Bytecode(lib)
			if err != nil {
				return err
			}

			// Create call frame.
			lib.addInstr(choice.From, OpPushF, nil, 0)

			// Push argument scope.
			lib.addInstr(choice.From, OpPushS, nil, 1)

			// Set argument.
			lib.addInstr(choice.From, OpLocal, nil, choice.FuncValueFrame.Index)
			lib.addInstr(choice.From, OpLocalSet, nil, choice.FuncArgsFrame.Index)

			lib.addCall(nil, 1, ast.Tail)
			if !ast.Tail {
				// Pop value scope.
				lib.addPopS(choice.From, 1, ast.Captures)
			}
		} else {
			// Compile expressions.
			for _, expr := range choice.Exprs {
				err := expr.Bytecode(lib)
				if err != nil {
					return err
				}
			}
		}

		// Jump to end.
		instr := lib.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I
	}
	lib.addLabel(labelEnd)

	return nil
}

// ASTCase implements case syntax.
type ASTCase struct {
	From        Locator
	Choices     []*ASTCaseChoice
	ValueFrame  *EnvFrame
	EqvArgFrame *EnvFrame
	Expr        AST
	Tail        bool
	Captures    bool
}

// ASTCaseChoice implements a case choice.
type ASTCaseChoice struct {
	From      Locator
	Datums    []Value
	DatumLocs []Locator
	Exprs     []AST
}

// Equal test if this choice is equal to the argument choice.
func (c *ASTCaseChoice) Equal(o *ASTCaseChoice) bool {
	if len(c.Datums) != len(o.Datums) {
		return false
	}
	for idx, datum := range c.Datums {
		if !datum.Equal(c.Datums[idx]) {
			return false
		}
	}
	if len(c.Exprs) != len(o.Exprs) {
		return false
	}
	for idx, expr := range c.Exprs {
		if !expr.Equal(o.Exprs[idx]) {
			return false
		}
	}
	return true
}

// Locator implements AST.Locator.
func (ast *ASTCase) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTCase) Equal(o AST) bool {
	oast, ok := o.(*ASTCase)
	if !ok {
		return false
	}
	if len(ast.Choices) != len(oast.Choices) {
		return false
	}
	for idx, choice := range ast.Choices {
		if !choice.Equal(oast.Choices[idx]) {
			return false
		}
	}
	return ast.Expr.Equal(oast.Expr) && ast.Tail == oast.Tail &&
		ast.Captures == oast.Captures
}

// Type implements AST.Type.
func (ast *ASTCase) Type() *types.Type {
	var t *types.Type
	var hadDefault bool

	for _, choice := range ast.Choices {
		// Else branch has empty list of datums.
		if len(choice.Datums) == 0 {
			hadDefault = true
		}
		t = types.Unify(t, choice.Exprs[len(choice.Exprs)-1].Type())
	}
	if !hadDefault {
		// No default so value of the last eqv? (false) is one valid
		// return value.
		t = types.Unify(t, types.Boolean)
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTCase) Typecheck(lib *Library, round int) error {
	err := ast.Expr.Typecheck(lib, round)
	if err != nil {
		return err
	}
	for _, choice := range ast.Choices {
		for _, expr := range choice.Exprs {
			err = expr.Typecheck(lib, round)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCase) Bytecode(lib *Library) error {
	labelEnd := lib.newLabel()

	// Push value scope.
	lib.addInstr(ast.From, OpPushS, nil, 1)

	// Compile key.
	err := ast.Expr.Bytecode(lib)
	if err != nil {
		return err
	}

	// Save value.
	lib.addInstr(ast.From, OpLocalSet, nil, ast.ValueFrame.Index)

	// Compile clauses

	var labelClause *Instr

	for i, choice := range ast.Choices {
		if labelClause != nil {
			lib.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(ast.Choices) {
			next = lib.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		// Datums are empty for the else form: (else expr1 expr2...)

		var labelExprs *Instr
		if len(choice.Datums) > 0 {
			labelExprs = lib.newLabel()

			// Compare datums: ((datum1 ...) expr1 expr2...)
			for idx, datum := range choice.Datums {
				// (eqv? value datum)
				from := choice.DatumLocs[idx]

				instr := lib.addInstr(from, OpGlobal, nil, 0)
				instr.Sym = lib.scm.Intern("eqv?")

				lib.addInstr(from, OpPushF, nil, 0)

				lib.addInstr(from, OpPushS, nil, 2)

				lib.addInstr(from, OpLocal, nil, ast.ValueFrame.Index)
				lib.addInstr(from, OpLocalSet, nil, ast.EqvArgFrame.Index)

				lib.addInstr(from, OpConst, datum, 0)
				lib.addInstr(from, OpLocalSet, nil, ast.EqvArgFrame.Index+1)

				lib.addCall(from, 2, false)

				instr = lib.addInstr(from, OpIf, nil, 0)
				instr.J = labelExprs.I
			}

			// No datum matched.
			instr := lib.addInstr(nil, OpJmp, nil, 0)
			instr.J = next.I
		}

		if labelExprs != nil {
			lib.addLabel(labelExprs)
		}

		// Compile expressions.
		for _, expr := range choice.Exprs {
			err := expr.Bytecode(lib)
			if err != nil {
				return err
			}
		}

		// Jump to end.
		instr := lib.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I
	}

	lib.addLabel(labelEnd)

	if !ast.Tail {
		// Pop value scope.
		lib.addPopS(nil, 1, ast.Captures)
	}

	return nil
}

// ASTAnd implements (and ...) syntax.
type ASTAnd struct {
	From  Locator
	Exprs []AST
}

// Locator implements AST.Locator.
func (ast *ASTAnd) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTAnd) Equal(o AST) bool {
	oast, ok := o.(*ASTAnd)
	if !ok {
		return false
	}
	for idx, expr := range ast.Exprs {
		if !expr.Equal(oast.Exprs[idx]) {
			return false
		}
	}
	return true
}

// Type implements AST.Type.
func (ast *ASTAnd) Type() *types.Type {
	if len(ast.Exprs) == 0 {
		return types.Boolean
	}
	var t *types.Type
	for _, expr := range ast.Exprs {
		t = types.Unify(t, expr.Type())
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTAnd) Typecheck(lib *Library, round int) error {
	for _, expr := range ast.Exprs {
		err := expr.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTAnd) Bytecode(lib *Library) error {
	if len(ast.Exprs) == 0 {
		lib.addInstr(ast.From, OpConst, Boolean(true), 0)
		return nil
	}

	labelEnd := lib.newLabel()
	for i := 0; i < len(ast.Exprs)-1; i++ {
		err := ast.Exprs[i].Bytecode(lib)
		if err != nil {
			return err
		}
		instr := lib.addInstr(ast.Exprs[i].Locator(), OpIfNot, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	err := ast.Exprs[len(ast.Exprs)-1].Bytecode(lib)
	if err != nil {
		return err
	}

	lib.addLabel(labelEnd)

	return nil
}

// ASTOr implements (or ...) syntax.
type ASTOr struct {
	From  Locator
	Exprs []AST
}

// Locator implements AST.Locator.
func (ast *ASTOr) Locator() Locator {
	return ast.From
}

// Equal implements AST.Equal.
func (ast *ASTOr) Equal(o AST) bool {
	oast, ok := o.(*ASTOr)
	if !ok {
		return false
	}
	for idx, expr := range ast.Exprs {
		if !expr.Equal(oast.Exprs[idx]) {
			return false
		}
	}
	return true
}

// Type implements AST.Type.
func (ast *ASTOr) Type() *types.Type {
	if len(ast.Exprs) == 0 {
		return types.Boolean
	}
	var t *types.Type
	for _, expr := range ast.Exprs {
		t = types.Unify(t, expr.Type())
		if t != nil && !t.IsA(types.Boolean) {
			// The first non-boolean value is the value of or.
			return t
		}
	}
	return t
}

// Typecheck implements AST.Type.
func (ast *ASTOr) Typecheck(lib *Library, round int) error {
	for _, expr := range ast.Exprs {
		err := expr.Typecheck(lib, round)
		if err != nil {
			return err
		}
	}
	return nil
}

// Bytecode implements AST.Bytecode.
func (ast *ASTOr) Bytecode(lib *Library) error {
	if len(ast.Exprs) == 0 {
		lib.addInstr(ast.From, OpConst, Boolean(false), 0)
		return nil
	}

	labelEnd := lib.newLabel()
	for i := 0; i < len(ast.Exprs)-1; i++ {
		err := ast.Exprs[i].Bytecode(lib)
		if err != nil {
			return err
		}
		instr := lib.addInstr(ast.Exprs[i].Locator(), OpIf, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	err := ast.Exprs[len(ast.Exprs)-1].Bytecode(lib)
	if err != nil {
		return err
	}

	lib.addLabel(labelEnd)

	return nil
}

func (lib *Library) define(loc Locator, name *Identifier, flags Flags) error {
	export, ok := lib.exported[name.Name]
	if ok {
		export.id = name
	}

	instr := lib.addInstr(loc, OpDefine, nil, int(flags))
	instr.Sym = lib.scm.Intern(name.Name)

	return nil
}

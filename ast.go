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
	Bytecode(c *Parser) error
}

// AbstractSyntaxTree defines AST as a Value.
type AbstractSyntaxTree struct {
	AST AST
}

// Scheme implements Value.Scheme.
func (ast *AbstractSyntaxTree) Scheme() string {
	return "#<ast>"
}

// Eq implements Value.Eq.
func (ast *AbstractSyntaxTree) Eq(o Value) bool {
	oast, ok := o.(*AbstractSyntaxTree)
	return ok && ast == oast
}

// Equal implements Value.Equal.
func (ast *AbstractSyntaxTree) Equal(o Value) bool {
	return ast.Eq(o)
}

// Type implements Value.Type.
func (ast *AbstractSyntaxTree) Type() *types.Type {
	return types.Any
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
		return nil
	}
	return ast.Items[len(ast.Items)-1].Type()
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSequence) Bytecode(c *Parser) error {
	for _, item := range ast.Items {
		err := item.Bytecode(c)
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

// Bytecode implements AST.Bytecode.
func (ast *ASTDefine) Bytecode(c *Parser) error {
	err := ast.Value.Bytecode(c)
	if err != nil {
		return err
	}

	// XXX Define symbol's type.

	instr := c.addInstr(ast.From, OpDefine, nil, int(ast.Flags))
	instr.Sym = c.scm.Intern(ast.Name.Name)
	return nil
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

// Bytecode implements AST.Bytecode.
func (ast *ASTSet) Bytecode(c *Parser) error {
	err := ast.Value.Bytecode(c)
	if err != nil {
		return err
	}

	if ast.Binding != nil {
		if ast.Binding.Frame.Type == TypeStack {
			c.addInstr(ast.From, OpLocalSet, nil,
				ast.Binding.Frame.Index+ast.Binding.Index)
		} else {
			instr := c.addInstr(ast.From, OpEnvSet, nil,
				ast.Binding.Frame.Index)
			instr.J = ast.Binding.Index
		}
	} else {
		instr := c.addInstr(ast.From, OpGlobalSet, nil, 0)
		instr.Sym = c.scm.Intern(ast.Name)
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

// Bytecode implements AST.Bytecode.
func (ast *ASTLet) Bytecode(c *Parser) error {
	c.addPushS(ast.From, len(ast.Bindings), ast.Captures)

	for _, binding := range ast.Bindings {
		err := binding.Init.Bytecode(c)
		if err != nil {
			return err
		}
		if binding.Binding.Frame.Type == TypeStack {
			c.addInstr(binding.From, OpLocalSet, nil,
				binding.Binding.Frame.Index+binding.Binding.Index)
		} else {
			instr := c.addInstr(binding.From, OpEnvSet, nil,
				binding.Binding.Frame.Index)
			instr.J = binding.Binding.Index
		}
	}

	for _, item := range ast.Body {
		err := item.Bytecode(c)
		if err != nil {
			return err
		}
	}
	if !ast.Tail {
		c.addPopS(nil, len(ast.Bindings), ast.Captures)
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

// Bytecode implements AST.Bytecode.
func (ast *ASTIf) Bytecode(c *Parser) error {
	labelFalse := c.newLabel()
	labelEnd := c.newLabel()

	err := ast.Cond.Bytecode(c)
	if err != nil {
		return err
	}

	if ast.False == nil {
		// (if cond t)
		instr := c.addInstr(ast.From, OpIfNot, nil, 0)
		instr.J = labelEnd.I

		err = ast.True.Bytecode(c)
		if err != nil {
			return err
		}
	} else {
		// (if cond t f)
		instr := c.addInstr(ast.From, OpIfNot, nil, 0)
		instr.J = labelFalse.I

		err = ast.True.Bytecode(c)
		if err != nil {
			return err
		}
		instr = c.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I

		c.addLabel(labelFalse)
		err = ast.False.Bytecode(c)
		if err != nil {
			return err
		}
	}
	c.addLabel(labelEnd)

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
	if t == nil {
		return nil
	}
	return t.Return
}

// Bytecode implements AST.Bytecode.
func (ast *ASTApply) Bytecode(c *Parser) error {
	err := ast.Lambda.Bytecode(c)
	if err != nil {
		return err
	}

	// Create a call frame.
	c.addInstr(nil, OpPushF, nil, 0)

	// Compile arguments.
	err = ast.Args.Bytecode(c)
	if err != nil {
		return err
	}

	// Push apply scope.
	c.addInstr(nil, OpPushA, nil, 0)

	c.addCall(nil, -1, ast.Tail)

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
		Car:  types.Any,
		Cdr:  types.Any,
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

// Bytecode implements AST.Bytecode.
func (ast *ASTCall) Bytecode(c *Parser) error {
	if !ast.Inline {
		err := ast.Func.Bytecode(c)
		if err != nil {
			return nil
		}
		// Create call frame.
		c.addInstr(ast.From, OpPushF, nil, 0)
	}

	// Push argument scope.
	c.addInstr(ast.From, OpPushS, nil, len(ast.Args))

	// Evaluate arguments.
	for idx, arg := range ast.Args {
		err := arg.Bytecode(c)
		if err != nil {
			return err
		}
		c.addInstr(ast.ArgLocs[idx], OpLocalSet, nil, ast.ArgFrame.Index+idx)
	}

	if ast.Inline {
		c.addInstr(ast.From, ast.InlineOp, nil, 0)
		c.addInstr(ast.From, OpPopS, nil, len(ast.Args))
	} else {
		c.addCall(nil, len(ast.Args), ast.Tail)
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
	OpCar:   types.Any,
	OpCdr:   types.Any,
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

// Bytecode implements AST.Bytecode.
func (ast *ASTCallUnary) Bytecode(c *Parser) error {
	err := ast.Arg.Bytecode(c)
	if err != nil {
		return err
	}
	c.addInstr(ast.From, ast.Op, nil, 0)

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

// Bytecode implements AST.Bytecode.
func (ast *ASTLambda) Bytecode(c *Parser) error {
	c.addInstr(ast.From, OpLambda, nil, len(c.lambdas))
	c.lambdas = append(c.lambdas, &lambdaCompilation{
		Name:     ast.Name,
		Args:     ast.Args,
		Body:     ast.Body,
		Env:      ast.Env,
		Captures: ast.Captures,
	})
	if ast.Define {
		// XXX Define symbol's type.
		err := c.define(ast.From, ast.Env, ast.Name, ast.Flags)
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
		// XXX nil type needed...
		return types.Any
	}
	return ast.Value.Type()
}

// Bytecode implements AST.Bytecode.
func (ast *ASTConstant) Bytecode(c *Parser) error {
	c.addInstr(nil, OpConst, ast.Value, 0)
	return nil
}

// ASTIdentifier implements identifer references.
type ASTIdentifier struct {
	From    Locator
	Name    string
	Binding *EnvBinding
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
	// XXX
	return types.Unspecified
}

// Bytecode implements AST.Bytecode.
func (ast *ASTIdentifier) Bytecode(c *Parser) error {
	if ast.Binding != nil {
		if ast.Binding.Frame.Type == TypeStack {
			c.addInstr(ast.From, OpLocal, nil,
				ast.Binding.Frame.Index+ast.Binding.Index)
		} else {
			instr := c.addInstr(ast.From, OpEnv, nil, ast.Binding.Frame.Index)
			instr.J = ast.Binding.Index
		}
	} else {
		instr := c.addInstr(ast.From, OpGlobal, nil, 0)
		instr.Sym = c.scm.Intern(ast.Name)
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
		t = types.Unify(t, choice.Exprs[len(choice.Exprs)-1].Type())
	}
	if !hadDefault {
		// No default so value of the last cond (false) is one valid
		// return value.
		t = types.Unify(t, types.Boolean)
	}
	return t
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCond) Bytecode(c *Parser) error {
	labelEnd := c.newLabel()

	var labelClause *Instr

	for i, choice := range ast.Choices {
		if labelClause != nil {
			c.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(ast.Choices) {
			next = c.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		// The choice.Cond is nil for else case.
		if choice.Cond != nil {
			// Compile condition.
			err := choice.Cond.Bytecode(c)
			if err != nil {
				return err
			}
			instr := c.addInstr(choice.From, OpIfNot, nil, 0)
			instr.J = next.I
		}
		// cond => func
		if choice.Func != nil {
			// Push value scope.
			c.addInstr(choice.From, OpPushS, nil, 1)

			// Save value
			c.addInstr(choice.From, OpLocalSet, nil,
				choice.FuncValueFrame.Index)

			// Compile function.
			err := choice.Func.Bytecode(c)
			if err != nil {
				return err
			}

			// Create call frame.
			c.addInstr(choice.From, OpPushF, nil, 0)

			// Push argument scope.
			c.addInstr(choice.From, OpPushS, nil, 1)

			// Set argument.
			c.addInstr(choice.From, OpLocal, nil, choice.FuncValueFrame.Index)
			c.addInstr(choice.From, OpLocalSet, nil, choice.FuncArgsFrame.Index)

			c.addCall(nil, 1, ast.Tail)
			if !ast.Tail {
				// Pop value scope.
				c.addPopS(choice.From, 1, ast.Captures)
			}
		} else {
			// Compile expressions.
			for _, expr := range choice.Exprs {
				err := expr.Bytecode(c)
				if err != nil {
					return err
				}
			}
		}

		// Jump to end.
		instr := c.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I
	}
	c.addLabel(labelEnd)

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

// Bytecode implements AST.Bytecode.
func (ast *ASTCase) Bytecode(c *Parser) error {
	labelEnd := c.newLabel()

	// Push value scope.
	c.addInstr(ast.From, OpPushS, nil, 1)

	// Compile key.
	err := ast.Expr.Bytecode(c)
	if err != nil {
		return err
	}

	// Save value.
	c.addInstr(ast.From, OpLocalSet, nil, ast.ValueFrame.Index)

	// Compile clauses

	var labelClause *Instr

	for i, choice := range ast.Choices {
		if labelClause != nil {
			c.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(ast.Choices) {
			next = c.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		// Datums are empty for the else form: (else expr1 expr2...)

		var labelExprs *Instr
		if len(choice.Datums) > 0 {
			labelExprs = c.newLabel()

			// Compare datums: ((datum1 ...) expr1 expr2...)
			for idx, datum := range choice.Datums {
				// (eqv? value datum)
				from := choice.DatumLocs[idx]

				instr := c.addInstr(from, OpGlobal, nil, 0)
				instr.Sym = c.scm.Intern("eqv?")

				c.addInstr(from, OpPushF, nil, 0)

				c.addInstr(from, OpPushS, nil, 2)

				c.addInstr(from, OpLocal, nil, ast.ValueFrame.Index)
				c.addInstr(from, OpLocalSet, nil, ast.EqvArgFrame.Index)

				c.addInstr(from, OpConst, datum, 0)
				c.addInstr(from, OpLocalSet, nil, ast.EqvArgFrame.Index+1)

				c.addCall(from, 2, false)

				instr = c.addInstr(from, OpIf, nil, 0)
				instr.J = labelExprs.I
			}

			// No datum matched.
			instr := c.addInstr(nil, OpJmp, nil, 0)
			instr.J = next.I
		}

		if labelExprs != nil {
			c.addLabel(labelExprs)
		}

		// Compile expressions.
		for _, expr := range choice.Exprs {
			err := expr.Bytecode(c)
			if err != nil {
				return err
			}
		}

		// Jump to end.
		instr := c.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I
	}

	c.addLabel(labelEnd)

	if !ast.Tail {
		// Pop value scope.
		c.addPopS(nil, 1, ast.Captures)
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

// Bytecode implements AST.Bytecode.
func (ast *ASTAnd) Bytecode(c *Parser) error {
	if len(ast.Exprs) == 0 {
		c.addInstr(ast.From, OpConst, Boolean(true), 0)
		return nil
	}

	labelEnd := c.newLabel()
	for i := 0; i < len(ast.Exprs)-1; i++ {
		err := ast.Exprs[i].Bytecode(c)
		if err != nil {
			return err
		}
		instr := c.addInstr(ast.Exprs[i].Locator(), OpIfNot, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	err := ast.Exprs[len(ast.Exprs)-1].Bytecode(c)
	if err != nil {
		return err
	}

	c.addLabel(labelEnd)

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

// Bytecode implements AST.Bytecode.
func (ast *ASTOr) Bytecode(c *Parser) error {
	if len(ast.Exprs) == 0 {
		c.addInstr(ast.From, OpConst, Boolean(false), 0)
		return nil
	}

	labelEnd := c.newLabel()
	for i := 0; i < len(ast.Exprs)-1; i++ {
		err := ast.Exprs[i].Bytecode(c)
		if err != nil {
			return err
		}
		instr := c.addInstr(ast.Exprs[i].Locator(), OpIf, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	err := ast.Exprs[len(ast.Exprs)-1].Bytecode(c)
	if err != nil {
		return err
	}

	c.addLabel(labelEnd)

	return nil
}

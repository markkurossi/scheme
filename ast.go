//
// Copyright (c) 2023-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/pp"
	"github.com/markkurossi/scheme/types"
)

// AST defines an abstract syntax tree entry
type AST interface {
	Locator() Locator
	SetInferTypes(it *InferTypes)
	InferTypes() *InferTypes
	SetType(t *types.Type)
	Type() *types.Type
	Infer(env *InferEnv) (*InferBranch, *InferTypes, error)
	Inferred(env *InferEnv) error
	Bytecode(lib *Library) error
	PP(w pp.Writer)
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
	_ AST = &ASTSymbol{}
	_ AST = &ASTCond{}
	_ AST = &ASTCase{}
	_ AST = &ASTAnd{}
	_ AST = &ASTOr{}
	_ AST = &ASTPragma{}
	_ AST = &ASTMacro{}
)

// Typed implements AST type information.
type Typed struct {
	t  *types.Type
	it *InferTypes
}

// SetType implements AST.SetType.
func (t *Typed) SetType(typ *types.Type) {
	t.t = typ
}

// Type implements AST.Type.
func (t *Typed) Type() *types.Type {
	if t.t == nil {
		return types.Unspecified
	}
	return t.t
}

// SetInferTypes implements AST.SetInferTypes.
func (t *Typed) SetInferTypes(it *InferTypes) {
	t.it = it
}

// InferTypes implements AST.InferTypes.
func (t *Typed) InferTypes() *InferTypes {
	return t.it
}

// ASTSequence implements a (begin ...) sequence.
type ASTSequence struct {
	Typed
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
	Typed
	From  Locator
	Name  *Symbol
	Flags Flags
	Value AST
}

// Locator implements AST.Locator.
func (ast *ASTDefine) Locator() Locator {
	return ast.From
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
	Typed
	From    Locator
	Name    string
	Binding *EnvBinding
	Value   AST
}

// Locator implements AST.Locator.
func (ast *ASTSet) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSet) Bytecode(lib *Library) error {
	err := ast.Value.Bytecode(lib)
	if err != nil {
		return err
	}

	if ast.Binding != nil {
		lib.setBinding(ast.From, ast.Binding)
	} else {
		instr := lib.addInstr(ast.From, OpGlobalSet, nil, 0)
		instr.Sym = lib.scm.Intern(ast.Name)
	}

	return nil
}

// ASTLet implements let syntaxes.
type ASTLet struct {
	Typed
	From     Locator
	Kind     LetType
	Captures bool
	Tail     bool
	Bindings []*ASTLetBinding
	Body     []AST
}

// LetType specifies the let type.
type LetType byte

func (lt LetType) String() string {
	switch lt {
	case Let:
		return "let"
	case LetStar:
		return "let*"
	case Letrec:
		return "letrec"
	default:
		panic("invalid LetType")
	}
}

// Let types.
const (
	Let LetType = iota
	LetStar
	Letrec
)

// ASTLetBinding implements a let binding.
type ASTLetBinding struct {
	From    Locator
	Binding *EnvBinding
	Init    AST
}

// Name returns the variable name of the let binding.
func (b *ASTLetBinding) Name() string {
	for k, v := range b.Binding.Frame.Bindings {
		if v == b.Binding {
			return k
		}
	}
	panic(fmt.Sprintf("let binding's name not found"))
}

// Locator implements AST.Locator.
func (ast *ASTLet) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLet) Bytecode(lib *Library) error {
	lib.addPushS(ast.From, len(ast.Bindings), ast.Captures)

	for _, binding := range ast.Bindings {
		err := binding.Init.Bytecode(lib)
		if err != nil {
			return err
		}
		lib.setBinding(binding.From, binding.Binding)
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
	Typed
	From  Locator
	Cond  AST
	True  AST
	False AST
}

// Locator implements AST.Locator.
func (ast *ASTIf) Locator() Locator {
	return ast.From
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
	Typed
	From   Locator
	Lambda AST
	Args   AST
	Tail   bool
}

// Locator implements AST.Locator.
func (ast *ASTApply) Locator() Locator {
	return ast.From
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
	Typed
	From     Locator
	Inline   bool
	InlineOp Operand
	Func     AST
	ArgFrame *EnvFrame
	Args     []AST
	ArgLocs  []Locator
	Tail     bool
}

func (ast *ASTCall) String() string {
	str := "("
	if ast.Inline {
		str += ast.InlineOp.String()
	} else {
		str += fmt.Sprintf("%v", ast.Func)
	}
	for _, arg := range ast.Args {
		str += fmt.Sprintf(" %v", arg)
	}
	return str + ")"
}

// Locator implements AST.Locator.
func (ast *ASTCall) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCall) Bytecode(lib *Library) error {
	var self *lambdaCompilation
	const calltAsJump bool = false

	if !ast.Inline {
		sym, ok := ast.Func.(*ASTSymbol)
		if ok && sym.Binding != nil && lib.current != nil &&
			lib.current.Self == sym.Init {
			self = lib.current
		}

		err := ast.Func.Bytecode(lib)
		if err != nil {
			return nil
		}
		// Create call frame.
		if !(calltAsJump && self != nil && ast.Tail) {
			lib.addInstr(ast.From, OpPushF, nil, 0)
		}
	}

	// Push argument scope.
	if len(ast.Args) > 0 {
		lib.addInstr(ast.From, OpPushS, nil, len(ast.Args))
	}

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
	} else if calltAsJump && self != nil && ast.Tail {
		if false {
			fmt.Printf(" - jmp %v\n", self.Label)
			for idx, b := range self.ArgBindings {
				fmt.Printf("   - %v: %d.%d(%v) %v\n",
					idx, b.Frame.Index, b.Index, b.Disabled, b.Type)
			}
			self.Env.Print()
		}
		instr := lib.addInstr(nil, OpJmp, nil, 0)
		instr.J = self.Label.I
	} else {
		lib.addCall(nil, len(ast.Args), ast.Tail)
	}

	return nil
}

// ASTCallUnary implements inlined unary function calls.
type ASTCallUnary struct {
	Typed
	From Locator
	Op   Operand
	I    int
	Arg  AST
}

// Locator implements AST.Locator.
func (ast *ASTCallUnary) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCallUnary) Bytecode(lib *Library) error {
	err := ast.Arg.Bytecode(lib)
	if err != nil {
		return err
	}
	lib.addInstr(ast.From, ast.Op, nil, ast.I)

	return nil
}

// ASTLambda implements lambda syntax.
type ASTLambda struct {
	Typed
	From        Locator
	Name        *Symbol
	Args        Args
	ArgBindings []*EnvBinding
	Body        []AST
	Env         *Env
	Captures    bool
	Define      bool
	Flags       Flags
}

func (ast *ASTLambda) String() string {
	result := "\u03bb"

	if ast.Name != nil {
		result += fmt.Sprintf("[%s]", ast.Name)
	}
	result += ast.Args.String()

	return result + "?"
}

// Locator implements AST.Locator.
func (ast *ASTLambda) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLambda) Bytecode(lib *Library) error {
	lib.addInstr(ast.From, OpLambda, nil, len(lib.lambdas))
	lib.lambdas = append(lib.lambdas, &lambdaCompilation{
		Self:        ast,
		Name:        ast.Name,
		Args:        ast.Args,
		ArgBindings: ast.ArgBindings,
		Body:        ast.Body,
		Env:         ast.Env,
		Captures:    ast.Captures,
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
	From        Locator
	Quoted      bool
	Value       Value
	LexicalType *types.Type
}

func (ast *ASTConstant) String() string {
	return ToString(ast.Value)
}

// Locator implements AST.Locator.
func (ast *ASTConstant) Locator() Locator {
	return ast.From
}

// SetInferTypes implements AST.SetInferTypes.
func (ast *ASTConstant) SetInferTypes(it *InferTypes) {
}

// InferTypes implements AST.InferTypes.
func (ast *ASTConstant) InferTypes() *InferTypes {
	return &InferTypes{
		Conclusive: true,
		Types:      []*types.Type{ast.Type()},
	}
}

// SetType implements AST.SetType
func (ast *ASTConstant) SetType(t *types.Type) {
}

// Type implements AST.Type.
func (ast *ASTConstant) Type() *types.Type {
	if ast.Value == nil {
		return types.Nil
	}
	return ast.Value.Type()
}

// Bytecode implements AST.Bytecode.
func (ast *ASTConstant) Bytecode(lib *Library) error {
	lib.addInstr(nil, OpConst, ast.Value, 0)
	return nil
}

// ASTSymbol implements symbol references.
type ASTSymbol struct {
	Typed
	From    Locator
	Name    string
	Binding *EnvBinding
	Global  *Symbol
	Init    AST
}

func (ast *ASTSymbol) String() string {
	return ast.Name
}

// Locator implements AST.Locator.
func (ast *ASTSymbol) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSymbol) Bytecode(lib *Library) error {
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
	Typed
	From       Locator
	Choices    []*ASTCondChoice
	Conclusive bool
	Tail       bool
	Captures   bool
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

// Locator implements AST.Locator.
func (ast *ASTCond) Locator() Locator {
	return ast.From
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
			lib.addInstr(choice.From, OpLocalSet, nil,
				choice.FuncArgsFrame.Index)

			lib.addCall(nil, 1, ast.Tail)
			if !ast.Tail {
				// Pop value scope.
				lib.addPopS(choice.From, 1, false)
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
	Typed
	From        Locator
	Choices     []*ASTCaseChoice
	ValueFrame  *EnvFrame
	EqvArgFrame *EnvFrame
	Expr        AST
	Conclusive  bool
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

// Locator implements AST.Locator.
func (ast *ASTCase) Locator() Locator {
	return ast.From
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
		lib.addPopS(nil, 1, false)
	}

	return nil
}

// ASTAnd implements (and ...) syntax.
type ASTAnd struct {
	Typed
	From  Locator
	Exprs []AST
}

// Locator implements AST.Locator.
func (ast *ASTAnd) Locator() Locator {
	return ast.From
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
	Typed
	From  Locator
	Exprs []AST
}

// Locator implements AST.Locator.
func (ast *ASTOr) Locator() Locator {
	return ast.From
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

// ASTPragma implements compiler pragmas.
type ASTPragma struct {
	Typed
	From       Locator
	Directives [][]Value
}

// Locator implements AST.Locator.
func (ast *ASTPragma) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTPragma) Bytecode(lib *Library) error {
	return nil
}

// ASTMacro implements scheme macros.
type ASTMacro struct {
	Typed
	From      Locator
	Scope     MacroScope
	Kind      MacroKind
	Symbol    *Symbol
	Literals  map[string]*Symbol
	Variables map[string]*Symbol
}

// MacroScope defines macro scopes.
type MacroScope int

// Macro scopes.
const (
	MacroDefine MacroScope = iota
	MacroLet
	MacroLetrec
)

var macroScopeNames = map[MacroScope]string{
	MacroDefine: "define-syntax",
	MacroLet:    "let-syntax",
	MacroLetrec: "letrec-syntax",
}

func (scope MacroScope) String() string {
	name, ok := macroScopeNames[scope]
	if ok {
		return name
	}
	panic("unknown MacroScope")
}

// MacroKind specifies the macro type.
type MacroKind int

// Macro types.
const (
	MacroSyntaxRules MacroKind = iota
	MacroIdentifierSyntax
	MacroSyntaxCase
)

// Locator implements AST.Locator.
func (ast *ASTMacro) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTMacro) Bytecode(lib *Library) error {
	return nil
}

func (lib *Library) define(loc Locator, name *Symbol, flags Flags) error {
	export, ok := lib.exported[name.Name]
	if ok {
		export.id = name
	}

	instr := lib.addInstr(loc, OpDefine, nil, int(flags))
	instr.Sym = lib.scm.Intern(name.Name)

	return nil
}

//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

// AST defines an abstract syntax tree entry
type AST interface {
	Locator() Locator
	Bytecode(c *Compiler) error
}

var (
	_ AST = &ASTSequence{}
	_ AST = &ASTDefine{}
	_ AST = &ASTLambda{}
	_ AST = &ASTConstant{}
	_ AST = &ASTIdentifier{}
	_ AST = &ASTCond{}
	_ AST = &ASTCase{}
)

// ASTSequence implements a (begin ...) sequence.
type ASTSequence struct {
	From  Locator
	Items []AST
}

// Locator implements AST.Locator.
func (ast *ASTSequence) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTSequence) Bytecode(c *Compiler) error {
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

// Bytecode implements AST.Bytecode.
func (ast *ASTDefine) Bytecode(c *Compiler) error {
	err := ast.Value.Bytecode(c)
	if err != nil {
		return err
	}

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

// Bytecode implements AST.Bytecode.
func (ast *ASTSet) Bytecode(c *Compiler) error {
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
	Type     Keyword
	Bindings []ASTLetBinding
	Body     []AST
}

// ASTLetBinding implements a let binding.
type ASTLetBinding struct {
	From    Locator
	Name    *Identifier
	Binding *EnvBinding
	Value   AST
}

// Locator implements AST.Locator.
func (ast *ASTLet) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLet) Bytecode(c *Compiler) error {
	for _, binding := range ast.Bindings {
		err := binding.Value.Bytecode(c)
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

// Bytecode implements AST.Bytecode.
func (ast *ASTIf) Bytecode(c *Compiler) error {
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

// Bytecode implements AST.Bytecode.
func (ast *ASTApply) Bytecode(c *Compiler) error {
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

// ASTLambda implements lambda syntax.
type ASTLambda struct {
	From     Locator
	Name     string
	Args     Args
	Body     []AST
	Capture  *Env
	Captures bool
}

// Locator implements AST.Locator.
func (ast *ASTLambda) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTLambda) Bytecode(c *Compiler) error {
	c.addInstr(ast.From, OpLambda, nil, len(c.lambdas))
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

// Bytecode implements AST.Bytecode.
func (ast *ASTConstant) Bytecode(c *Compiler) error {
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

// Bytecode implements AST.Bytecode.
func (ast *ASTIdentifier) Bytecode(c *Compiler) error {
	if ast.Binding != nil {
		if ast.Binding.Frame.Type == TypeStack {
			c.addInstr(ast.From, OpLocal, nil,
				ast.Binding.Frame.Index+ast.Binding.Index)
		} else {
			instr := c.addInstr(ast.From, OpEnv, nil,
				ast.Binding.Frame.Index)
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
	Choices  []ASTCondChoice
	Tail     bool
	Captures bool
}

// ASTCondChoice implements a cond choice.
type ASTCondChoice struct {
	From      Locator
	Cond      AST
	Func      AST
	FuncValue *EnvFrame
	FuncArgs  *EnvFrame
	Exprs     []AST
}

// Locator implements AST.Locator.
func (ast *ASTCond) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCond) Bytecode(c *Compiler) error {
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
			c.addInstr(choice.From, OpLocalSet, nil, choice.FuncValue.Index)

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
			c.addInstr(choice.From, OpLocal, nil, choice.FuncValue.Index)
			c.addInstr(choice.From, OpLocalSet, nil, choice.FuncArgs.Index)

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
	Choices     []ASTCaseChoice
	ValueFrame  *EnvFrame
	EqvArgFrame *EnvFrame
	Expr        AST
	Tail        bool
	Captures    bool
}

// ASTCaseChoice implements a case choice.
type ASTCaseChoice struct {
	From           Locator
	Datums         []Value
	DatumLocations []Locator
	Exprs          []AST
}

// Locator implements AST.Locator.
func (ast *ASTCase) Locator() Locator {
	return ast.From
}

// Bytecode implements AST.Bytecode.
func (ast *ASTCase) Bytecode(c *Compiler) error {
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
				from := choice.DatumLocations[idx]

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

// Bytecode implements AST.Bytecode.
func (ast *ASTAnd) Bytecode(c *Compiler) error {
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

// Bytecode implements AST.Bytecode.
func (ast *ASTOr) Bytecode(c *Compiler) error {
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

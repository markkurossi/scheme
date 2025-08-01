//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"github.com/markkurossi/scheme/pp"
	"github.com/markkurossi/scheme/types"
)

// PP implements AST.PP.
func (ast *ASTSequence) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("begin")
	w.Println()
	w.PushN(2)
	for idx, item := range ast.Items {
		item.PP(w)
		if idx+1 >= len(ast.Items) {
			w.Printf(")")
			w.Type(ast.Type().String())
		} else {
			w.Println()
		}
	}
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTDefine) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("define")
	w.Printf(" ")
	w.Name(ast.Name.String())
	w.Printf(" ")
	ast.Value.PP(w)
	w.Printf(")")
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTSet) PP(w pp.Writer) {
	w.Printf("(set! ")
	w.Name(ast.Name)
	w.Printf(" ")
	ast.Value.PP(w)
	w.Printf(")")
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTLet) PP(w pp.Writer) {
	w.Push()

	w.Printf("(")
	w.Keyword(ast.Kind.String())
	w.Printf(" (")

	w.Push()

	// Count the length of the longest identifier.
	var longest int
	for _, b := range ast.Bindings {
		l := len(b.Name())
		if l > longest {
			longest = l
		}
	}

	for idx, b := range ast.Bindings {
		name := b.Name()
		w.Printf("(%s ", name)

		for i := len(name); i < longest; i++ {
			w.Printf(" ")
		}

		b.Init.PP(w)
		if idx+1 >= len(ast.Bindings) {
			w.Println("))")
		} else {
			w.Println(")")
		}
	}
	w.Pop()
	w.PushN(2)
	for idx, b := range ast.Body {
		b.PP(w)
		if idx+1 >= len(ast.Body) {
			w.Printf(")")
			w.Type(ast.Type().String())
		} else {
			w.Println()
		}
	}
	w.Pop()
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTIf) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("if")
	w.Printf(" ")
	w.PushN(4)
	ast.Cond.PP(w)
	w.Println()
	ast.True.PP(w)

	if ast.False != nil {
		w.Println()
		ast.False.PP(w)
	}
	w.Printf(")")
	w.Type(ast.Type().String())
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTApply) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTCall) PP(w pp.Writer) {
	w.Printf("(")
	if ast.Inline {
		w.Printf("%s", ast.InlineOp)
	} else {
		ast.Func.PP(w)
	}
	for _, arg := range ast.Args {
		w.Printf(" ")
		arg.PP(w)
	}
	w.Printf(")")
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTCallUnary) PP(w pp.Writer) {
	var op string

	switch ast.Op {
	case OpAddConst:
		op = "+"

	case OpSubConst:
		op = "-"

	case OpMulConst:
		op = "*"

	default:
		op = ast.Op.String()
	}
	w.Printf("(%s ", op)
	ast.Arg.PP(w)

	switch ast.Op {
	case OpAddConst, OpSubConst, OpMulConst:
		w.Printf(" %v", ast.I)
		w.Type("int")
	}

	w.Printf(")")
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTLambda) PP(w pp.Writer) {
	w.Push()
	defer w.Pop()

	w.Printf("(")
	if ast.Define {
		w.Keyword("define")
		w.Printf(" (")
		w.Name(ast.Name.String())
		w.Printf(" ")
	} else {
		w.Keyword("lambda")
		w.Printf(" (")
	}
	for idx, arg := range ast.Args.Fixed {
		if idx > 0 {
			w.Printf(" ")
		}
		w.Printf("%s", arg.Name)
	}
	if ast.Args.Rest != nil {
		w.Printf(" . %s", ast.Args.Rest.Name)
	}
	w.Println(")")
	w.PushN(2)
	for idx, b := range ast.Body {
		b.PP(w)
		if idx+1 >= len(ast.Body) {
			w.Printf(")")
			w.Type(ast.Type().String())
		} else {
			w.Println()
		}
	}
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTConstant) PP(w pp.Writer) {
	if ast.Quoted {
		w.Printf("'")
	}
	switch v := ast.Value.(type) {
	case String:
		w.Literal(ToScheme(v))
	default:
		if ast.LexicalType != nil &&
			(ast.LexicalType.IsA(types.InexactInteger) ||
				ast.LexicalType.IsA(types.InexactFloat)) {
			w.Printf("#i%s", ToString(ast.Value))
		} else {
			w.Printf("%s", ToScheme(ast.Value))
		}
	}
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTSymbol) PP(w pp.Writer) {
	w.Printf("%s", ast.Name)
	w.Type(ast.Type().String())
}

// PP implements AST.PP.
func (ast *ASTCond) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("cond")
	w.Printf(" ")
	w.PushN(6)

	for i, choice := range ast.Choices {
		w.Printf("(")
		if choice.Cond == nil {
			w.Keyword("else")
		} else {
			choice.Cond.PP(w)
		}
		if choice.Func != nil {
			w.Printf(" => ")
			choice.Func.PP(w)
		} else {
			for _, expr := range choice.Exprs {
				w.Printf(" ")
				expr.PP(w)
			}
		}
		w.Printf(")")
		if i+1 >= len(ast.Choices) {
			w.Printf(")")
			w.Type(ast.Type().String())
		} else {
			w.Println()
		}
	}
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTCase) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("case")
	w.Printf(" ")
	ast.Expr.PP(w)
	w.Println()
	w.PushN(2)

	for i, choice := range ast.Choices {
		lastLine := choice.From.From().Line

		w.Printf("(")
		if len(choice.Datums) == 0 {
			w.Keyword("else")
		} else {
			w.Printf("(")
			for j, datum := range choice.Datums {
				if j > 0 {
					w.Printf(" ")
				}
				w.Printf("%v", datum)
			}
			w.Printf(")")
		}
		w.Printf(" ")
		w.PushN(1)

		for j, expr := range choice.Exprs {
			line := expr.Locator().From().Line
			if line != lastLine {
				w.Println()
			} else if j > 0 {
				w.Printf(" ")
			}
			expr.PP(w)
		}
		w.Printf(")")
		w.Pop()

		if i+1 >= len(ast.Choices) {
			w.Printf(")")
			w.Type(ast.Type().String())
		} else {
			w.Println()
		}
	}

	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTAnd) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("and")

	var indented bool
	lastLine := ast.Locator().From().Line

	for _, expr := range ast.Exprs {
		line := expr.Locator().From().Line
		if line != lastLine {
			w.Println()
			if !indented {
				w.PushN(5)
				indented = true
			}
		} else {
			w.Printf(" ")
		}
		lastLine = line
		expr.PP(w)
	}
	w.Printf(")")
	w.Type(ast.Type().String())
	if indented {
		w.Pop()
	}
}

// PP implements AST.PP.
func (ast *ASTOr) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("or")

	var indented bool
	lastLine := ast.Locator().From().Line

	for _, expr := range ast.Exprs {
		line := expr.Locator().From().Line
		if line != lastLine {
			w.Println()
			if !indented {
				w.PushN(4)
				indented = true
			}
		} else {
			w.Printf(" ")
		}
		lastLine = line
		expr.PP(w)
	}
	w.Printf(")")
	w.Type(ast.Type().String())
	if indented {
		w.Pop()
	}
}

// PP implements AST.PP.
func (ast *ASTPragma) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword("pragma")
	w.Printf(" ")
	w.PushN(8)
	for i, d := range ast.Directives {
		if i > 0 {
			w.Println()
		}
		w.Printf("(")
		for j, v := range d {
			if j > 0 {
				w.Printf(" ")
			}
			w.Printf("%v", v)
		}
		w.Printf(")")
	}
	w.Printf(")")
	w.Pop()
}

// PP implements AST.PP.
func (ast *ASTMacro) PP(w pp.Writer) {
	w.Printf("(xxx macro xxx")
	w.Printf(")")
}

//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"github.com/markkurossi/scheme/pp"
)

// PP implements AST.PP.
func (ast *ASTSequence) PP(w pp.Writer) {
	for _, item := range ast.Items {
		item.PP(w)
	}
}

// PP implements AST.PP.
func (ast *ASTDefine) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTSet) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTLet) PP(w pp.Writer) {
	w.Printf("(")
	w.Keyword(ast.Kind.String())
	w.Printf(" (")

	indent := len(ast.Kind.String()) + 3
	w.Indent(indent)

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
	w.Indent(-indent)

	w.Indent(2)
	for idx, b := range ast.Body {
		b.PP(w)
		if idx+1 >= len(ast.Body) {
			w.Println(")")
		} else {
			w.Println()
		}
	}
	w.Indent(-2)
}

// PP implements AST.PP.
func (ast *ASTIf) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTApply) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTCall) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTCallUnary) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTLambda) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTConstant) PP(w pp.Writer) {
	var t string
	if ast.Value != nil {
		t = ast.Value.Type().String()
	}
	w.Typed(ToScheme(ast.Value), t)
}

// PP implements AST.PP.
func (ast *ASTIdentifier) PP(w pp.Writer) {
	w.Typed(ast.Name, "?")
}

// PP implements AST.PP.
func (ast *ASTCond) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTCase) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTAnd) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTOr) PP(w pp.Writer) {
}

// PP implements AST.PP.
func (ast *ASTPragma) PP(w pp.Writer) {
}

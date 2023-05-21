//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"io"

	"github.com/markkurossi/scheme/types"
)

// Parser implements the byte-code compiler.
type Parser struct {
	scm    *Scheme
	source string
}

type export struct {
	from Locator
	id   *Identifier
}

// NewParser creates a new bytecode compiler.
func NewParser(scm *Scheme) *Parser {
	return &Parser{
		scm: scm,
	}
}

// Parse parses the source.
func (p *Parser) Parse(source string, in io.Reader) (*Library, error) {
	sexpr := NewSexprParser(source, in)

	p.source = source
	p.scm.Parsing = true

	env := NewEnv()

	// Top-level definitions are executed inside an empty lambda so
	// push the empty argument frame.
	env.PushFrame(TypeEnv, FUArgs, 0)

	first := true

	library := &Library{
		scm:      p.scm,
		Source:   source,
		Body:     &ASTSequence{},
		exported: make(map[string]*export),
	}

	for {
		v, err := sexpr.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			}
			break
		}
		p.scm.Parsing = false

		// Check libraries.
		if first {
			pair, ok := v.(Pair)
			if ok && isNamedIdentifier(pair.Car(), "library") {
				list, ok := ListPairs(v)
				if !ok || len(list) < 4 {
					return nil, pair.Errorf("invalid library: %v", v)
				}
				err = library.parseLibraryHeader(list)
				if err != nil {
					return nil, err
				}

				// Compile library body.
				for i := 4; i < len(list); i++ {
					ast, err := p.parseValue(env, list[i], list[i].Car(), false,
						true)
					if err != nil {
						return nil, err
					}
					library.Body.Add(ast)
				}

				// Check that the file does not have any trailing garbage
				// after the library specification.
				v, err = sexpr.Next()
				if err == nil {
					return nil, fmt.Errorf("garbage after library: %v", v)
				}
				if err != io.EOF {
					return nil, err
				}
				break
			} else {
				// Not a library source.
				library.ExportAll = true
				library.Name = NewPair(&Identifier{
					Name: "main",
				}, nil)
			}
			first = false
		}

		ast, err := p.parseValue(env, Point{}, v, false, true)
		if err != nil {
			return nil, err
		}
		library.Body.Add(ast)
	}

	return library, nil
}

func (p *Parser) parseValue(env *Env, loc Locator, value Value,
	tail, captures bool) (AST, error) {

	switch v := value.(type) {
	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return nil, v.Errorf("unexpected value: %v", v)
		}
		length := len(list)

		if isKeyword(v.Car(), KwDefine) {
			return p.parseDefine(env, list, 0, captures)
		}
		if isKeyword(v.Car(), KwDefineConstant) {
			return p.parseDefine(env, list, FlagConst, captures)
		}
		if isKeyword(v.Car(), KwLambda) {
			return p.parseLambda(env, false, 0, list)
		}
		if isKeyword(v.Car(), KwSet) {
			return p.parseSet(env, list, captures)
		}
		if isKeyword(v.Car(), KwLet) {
			return p.parseLet(KwLet, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetStar) {
			return p.parseLet(KwLetStar, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetrec) {
			return p.parseLet(KwLetrec, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwBegin) {
			seq := &ASTSequence{
				From: loc,
			}
			err := MapPairs(func(idx int, pair Pair) error {
				ast, err := p.parseValue(env, pair, pair.Car(),
					tail && idx+1 >= length-1, captures)
				if err != nil {
					return err
				}
				seq.Add(ast)
				return nil
			}, v.Cdr())
			if err != nil {
				return nil, err
			}
			return seq, nil
		}
		if isKeyword(v.Car(), KwIf) {
			return p.parseIf(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwQuote) {
			if length != 2 {
				return nil, v.Errorf("invalid quote: %v", v)
			}
			quoted, ok := Car(v.Cdr(), true)
			if !ok {
				return nil, v.Errorf("invalid quote: %v", v)
			}
			return &ASTConstant{
				From:  loc,
				Value: quoted,
			}, nil
		}
		if isKeyword(v.Car(), KwSchemeApply) {
			if length != 3 {
				return nil, v.Errorf("invalid scheme::apply: %v", v)
			}
			return p.parseApply(env, v, tail, captures)
		}
		if isKeyword(v.Car(), KwCond) {
			return p.parseCond(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwCase) {
			return p.parseCase(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwAnd) {
			return p.parseAnd(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwOr) {
			return p.parseOr(env, list, tail, captures)
		}

		// Function call.

		// Unary inline functions.
		ok, inlineOp := p.inlineUnary(env, list)
		if ok {
			ast := &ASTCallUnary{
				From: list[0],
				Op:   inlineOp,
			}
			arg, err := p.parseValue(env, list[1], list[1].Car(), false,
				captures)
			if err != nil {
				return nil, err
			}
			ast.Arg = arg
			return ast, nil
		}

		// Other function calls.

		ast := &ASTCall{
			From: list[0],
			Tail: tail,
		}
		ok, inlineOp = p.inlineBinary(env, list)
		if ok {
			ast.Inline = true
			ast.InlineOp = inlineOp
		}

		// Environment for the lambda body when its arguments are
		// evaluated.
		lambdaEnv := env.Copy()

		if !ast.Inline {
			// Compile function.
			a, err := p.parseValue(env, list[0], list[0].Car(), false, captures)
			if err != nil {
				return nil, err
			}
			ast.Func = a

			// Create call frame.
			lambdaEnv.PushFrame(TypeStack, FUFrame, 1)
		}

		// Push argument scope.
		ast.ArgFrame = lambdaEnv.PushFrame(TypeStack, FUArgs, length-1)

		// Evaluate arguments.
		for i := 1; i < len(list); i++ {
			a, err := p.parseValue(lambdaEnv, list[i], list[i].Car(), false,
				captures)
			if err != nil {
				return nil, err
			}
			ast.Args = append(ast.Args, a)
			ast.ArgLocs = append(ast.ArgLocs, list[i])
		}

		return ast, nil

	case *Identifier:
		var sym *Identifier
		binding, ok := env.Lookup(v.Name)
		if !ok {
			sym = p.scm.Intern(v.Name)
		}
		return &ASTIdentifier{
			From:    loc,
			Name:    v.Name,
			Binding: binding,
			Global:  sym,
		}, nil

	case Keyword:
		return nil, loc.Errorf("unexpected keyword: %s", v)

	case Vector:
		// Vector literals must be quoted like list constants.
		return nil, loc.Errorf("invalid syntax: %v", v)

	case Bytevector, Boolean, String, Character, Int, *BigInt:
		return &ASTConstant{
			From:  loc,
			Value: v,
		}, nil

	default:
		return nil, fmt.Errorf("astValue: unsupported value: %v(%T)", v, v)
	}
}

var inlineUnary = map[string]Operand{
	"pair?": OpPairp,
	"car":   OpCar,
	"cdr":   OpCdr,
	"null?": OpNullp,
	"zero?": OpZerop,
	"not":   OpNot,
}

func (p *Parser) inlineUnary(env *Env, list []Pair) (bool, Operand) {

	if len(list) != 2 {
		return false, 0
	}
	id, ok := list[0].Car().(*Identifier)
	if !ok {
		return false, 0
	}
	op, ok := inlineUnary[id.Name]
	if !ok {
		return false, 0
	}

	return true, op
}

var inlineBinary = map[string]Operand{
	"cons": OpCons,
	"+":    OpAdd,
	"-":    OpSub,
	"=":    OpEq,
	"<":    OpLt,
	">":    OpGt,
	"<=":   OpLe,
	">=":   OpGe,
}

func (p *Parser) inlineBinary(env *Env, list []Pair) (bool, Operand) {

	if len(list) != 3 {
		return false, 0
	}
	id, ok := list[0].Car().(*Identifier)
	if !ok {
		return false, 0
	}
	op, ok := inlineBinary[id.Name]
	if !ok {
		return false, 0
	}

	return true, op
}

func (p *Parser) parseDefine(env *Env, list []Pair, flags Flags,
	captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("syntax error: %v", list[0])
	}
	// (define name value)
	name, ok := isIdentifier(list[1].Car())
	if ok {
		ast, err := p.parseValue(env, list[2], list[2].Car(), false, captures)
		if err != nil {
			return nil, err
		}
		return &ASTDefine{
			From:  list[1],
			Name:  name,
			Flags: flags,
			Value: ast,
		}, nil
	}

	// (define (name args?) body)
	return p.parseLambda(env, true, flags, list)
}

type seen map[string]bool

func newSeen() seen {
	return make(seen)
}

func (seen seen) add(name string) error {
	_, ok := seen[name]
	if ok {
		return fmt.Errorf("argument '%s' already seen", name)
	}
	seen[name] = true
	return nil
}

func (p *Parser) parseLambda(env *Env, define bool, flags Flags,
	list []Pair) (AST, error) {

	// (define (name args?) body)
	// (lambda (args?) body)
	// (lambda args body)
	if len(list) < 3 {
		return nil, list[0].Errorf("missing lambda body: %v", list[0])
	}

	var name *Identifier
	var args Args

	seen := newSeen()

	arg, ok := isIdentifier(list[1].Car())
	if ok {
		if define {
			return nil, list[1].Errorf("invalid define: %v", list[0])
		}
		err := seen.add(arg.Name)
		if err != nil {
			return nil, list[1].Errorf("%v", err)
		}
		args.Rest = &TypedName{
			Name: arg.Name,
			Type: types.Unspecified,
		}
	} else {
		var pair Pair
		if list[1].Car() != nil {
			pair, ok = list[1].Car().(Pair)
			if !ok {
				return nil, list[0].Errorf("invalid arguments: %v",
					list[1].Car())
			}
		}
		for pair != nil {
			arg, ok = isIdentifier(pair.Car())
			if !ok {
				return nil, pair.Errorf("invalid argument: %v", pair.Car())
			}
			if define && name == nil {
				name = arg
			} else {
				err := seen.add(arg.Name)
				if err != nil {
					return nil, pair.Errorf("%v", err)
				}
				args.Fixed = append(args.Fixed, &TypedName{
					Name: arg.Name,
					Type: types.Unspecified,
				})
			}

			arg, ok = isIdentifier(pair.Cdr())
			if ok {
				// Rest arguments.
				err := seen.add(arg.Name)
				if err != nil {
					return nil, fmt.Errorf("%s: %v", pair.To(), err)
				}
				args.Rest = &TypedName{
					Name: arg.Name,
					Type: types.Unspecified,
				}
				break
			}
			if pair.Cdr() == nil {
				pair = nil
			} else {
				next, ok := pair.Cdr().(Pair)
				if !ok {
					return nil, pair.Errorf("invalid argument: %v", pair)
				}
				pair = next
			}
		}
	}
	args.Init()

	if define && name == nil {
		return nil, list[0].Errorf("define: name not defined: %v", list[0])
	}

	// Check if the lambda captures the environment.
	var lambdas int
	var checkLambda func(idx int, p Pair) error

	var ErrNext = errors.New("next")

	checkLambda = func(idx int, p Pair) error {
		if idx == 0 && (isKeyword(p.Car(), KwLambda) ||
			isKeyword(p.Car(), KwDefine)) {
			lambdas++
			return ErrNext
		}
		car, ok := p.Car().(Pair)
		if !ok {
			return nil
		}
		return MapPairs(checkLambda, car)
	}

	body := list[2:]
	for _, pair := range body {
		MapPairs(checkLambda, pair)
	}
	captures := lambdas > 0

	// The environment (below) is converted to lambda capture:
	//
	//     0: let-frame-m
	//        ...
	//   n-1: let-frame-0
	//     n: ctx-function-args
	//
	// The final lambda environment (scope) is as follows. The lambda
	// argument frame is at the top of the capture environment:
	//
	//     0: lambda-args
	//     1: let-frame-m
	//        ...
	//   n-1: let-frame-0
	//     n: ctx-function-args

	numArgs := len(args.Fixed)
	if args.Rest != nil {
		numArgs++
	}

	capture := NewEnv()
	capture.Push(env)
	capture.PushCaptureFrame(captures, FUArgs, numArgs)

	for _, arg := range args.Fixed {
		_, err := capture.Define(arg.Name, types.Unspecified)
		if err != nil {
			return nil, err
		}
	}
	if args.Rest != nil {
		_, err := capture.Define(args.Rest.Name, types.Unspecified)
		if err != nil {
			return nil, err
		}
	}

	ast := &ASTLambda{
		From:     list[0],
		Name:     name,
		Args:     args,
		Env:      capture,
		Captures: captures,
		Define:   define,
		Flags:    flags,
	}

	for i := 2; i < len(list); i++ {
		a, err := p.parseValue(capture, list[i], list[i].Car(),
			i+1 >= len(list), captures)
		if err != nil {
			return nil, err
		}
		ast.Body = append(ast.Body, a)
	}

	return ast, nil
}

func (p *Parser) parseSet(env *Env, list []Pair, captures bool) (AST, error) {
	// (set! name value)
	if len(list) != 3 {
		return nil, list[0].Errorf("syntax error: %v", list[0])
	}
	name, ok := isIdentifier(list[1].Car())
	if !ok {
		return nil, list[1].Errorf("set!: expected variable name: %v",
			list[1].Car())
	}
	ast, err := p.parseValue(env, list[2], list[2].Car(), false, captures)
	if err != nil {
		return nil, err
	}

	binding, _ := env.Lookup(name.Name)

	return &ASTSet{
		From:    list[1],
		Name:    name.Name,
		Binding: binding,
		Value:   ast,
	}, nil
}

func (p *Parser) parseLet(kind Keyword, env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("%s: missing bindings or body", kind)
	}
	bindings, ok := ListPairs(list[1].Car())
	if !ok {
		return nil, list[1].Errorf("%s: invalid bindings: %v",
			kind, list[1].Car())
	}

	// XXX check if the let inits or body captures and use that as the
	// capture flag.

	letEnv := env.Copy()
	letEnv.PushCaptureFrame(captures, FULet, len(bindings))

	var letBindings []*EnvBinding

	for _, binding := range bindings {
		def, ok := ListPairs(binding.Car())
		if !ok || len(def) != 2 {
			return nil, list[1].Errorf("%s: invalid init: %v", kind, binding)
		}
		name, ok := def[0].Car().(*Identifier)
		if !ok {
			return nil, def[0].Errorf("%s: invalid variable: %v", kind, binding)
		}
		b, err := letEnv.Define(name.Name, types.Unspecified)
		if err != nil {
			return nil, err
		}
		if kind != KwLetrec {
			b.Disabled = true
		}
		letBindings = append(letBindings, b)
	}

	ast := &ASTLet{
		From:     list[0],
		Kind:     kind,
		Captures: captures,
		Tail:     tail,
	}

	for idx, binding := range bindings {
		def, ok := ListPairs(binding.Car())
		if !ok || len(def) != 2 {
			return nil, list[1].Errorf("%s: invalid init: %v", kind, binding)
		}

		initAst, err := p.parseValue(letEnv, def[1], def[1].Car(), false,
			captures)
		if err != nil {
			return nil, err
		}

		ast.Bindings = append(ast.Bindings, &ASTLetBinding{
			From:    def[1],
			Binding: letBindings[idx],
			Init:    initAst,
		})

		if kind == KwLetStar {
			letBindings[idx].Disabled = false
		}
	}

	if kind == KwLet {
		for i := 0; i < len(letBindings); i++ {
			letBindings[i].Disabled = false
		}
	}

	// Compile body.
	for i := 2; i < len(list); i++ {
		bodyAst, err := p.parseValue(letEnv, list[i], list[i].Car(),
			tail && i+1 >= len(list), captures)
		if err != nil {
			return nil, err
		}
		ast.Body = append(ast.Body, bodyAst)
	}

	return ast, nil
}

func (p *Parser) parseIf(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 3 || len(list) > 4 {
		return nil, list[0].Errorf("if: syntax error")
	}

	ast := &ASTIf{
		From: list[0],
	}
	a, err := p.parseValue(env, list[1], list[1].Car(), false, captures)
	if err != nil {
		return nil, err
	}
	ast.Cond = a

	a, err = p.parseValue(env, list[2], list[2].Car(), tail, captures)
	if err != nil {
		return nil, err
	}
	ast.True = a

	if len(list) == 4 {
		a, err = p.parseValue(env, list[3], list[3].Car(), tail, captures)
		if err != nil {
			return nil, err
		}
		ast.False = a
	}

	return ast, nil
}

func (p *Parser) parseApply(env *Env, pair Pair,
	tail, captures bool) (AST, error) {

	f, ok := Car(pair.Cdr(), true)
	if !ok {
		return nil, pair.Errorf("scheme::apply: invalid list: %v", pair)
	}
	args, ok := Car(Cdr(pair.Cdr(), true))
	if !ok {
		return nil, pair.Errorf("scheme::apply: invalid list: %v", pair)
	}

	// Lambda.
	lambdaAST, err := p.parseValue(env, pair, f, false, captures)
	if err != nil {
		return nil, err
	}

	// Create a call frame.
	env.PushFrame(TypeStack, FUFrame, 1)

	// Compile arguments.
	argsAST, err := p.parseValue(env, pair, args, false, captures)
	if err != nil {
		return nil, err
	}

	// Pop call frame.
	env.PopFrame()

	return &ASTApply{
		From:   pair,
		Lambda: lambdaAST,
		Args:   argsAST,
		Tail:   tail,
	}, nil
}

func (p *Parser) parseCond(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 2 {
		return nil, list[0].Errorf("cond: no clauses")
	}

	ast := &ASTCond{
		From:     list[0],
		Tail:     tail,
		Captures: captures,
	}

	for i := 1; i < len(list); i++ {
		clause, ok := ListPairs(list[i].Car())
		if !ok || len(clause) < 1 {
			return nil, list[i].Errorf("cond: invalid clause: %v", list[i])
		}

		var isElse bool
		if isKeyword(clause[0].Car(), KwElse) {
			if i+1 < len(list) {
				return nil,
					clause[0].Errorf("cond: else must be the last clause")
			}
			isElse = true
		}

		choice := &ASTCondChoice{
			From: clause[0],
		}
		ast.Choices = append(ast.Choices, choice)

		if !isElse {
			// Compile condition.
			condAST, err := p.parseValue(env, clause[0], clause[0].Car(), false,
				captures)
			if err != nil {
				return nil, err
			}
			choice.Cond = condAST
		}
		// cond => func
		if len(clause) > 1 && isKeyword(clause[1].Car(), KwImplies) {
			if len(clause) != 3 {
				return nil, clause[0].Errorf("cond: invalid => clause")
			}

			// Push value scope.
			choice.FuncValueFrame = env.PushCaptureFrame(false, FUValue, 1)

			// Compile function.
			funcAST, err := p.parseValue(env, clause[2], clause[2].Car(), false,
				captures)
			if err != nil {
				return nil, err
			}
			choice.Func = funcAST

			// Create call frame.
			env.PushCaptureFrame(captures, FUFrame, 1)

			// Push argument scope.
			choice.FuncArgsFrame = env.PushCaptureFrame(false, FUArgs, 1)

			env.PopFrame() // Argument
			env.PopFrame() // Call
			env.PopFrame() // Value
		} else {
			// Compile expressions.
			for j := 1; j < len(clause); j++ {
				last := j+1 >= len(clause)
				expr, err := p.parseValue(env, clause[j], clause[j].Car(),
					tail && last, captures)
				if err != nil {
					return nil, err
				}
				choice.Exprs = append(choice.Exprs, expr)
			}
		}
	}

	return ast, nil
}

func (p *Parser) parseCase(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("case: key or clauses")
	}

	ast := &ASTCase{
		From:     list[0],
		Tail:     tail,
		Captures: captures,
	}

	// Push value scope.
	ast.ValueFrame = env.PushCaptureFrame(false, FUValue, 1)

	// Compile key.
	expr, err := p.parseValue(env, list[1], list[1].Car(), false, captures)
	if err != nil {
		return nil, err
	}
	ast.Expr = expr

	// Compile clauses
	for i := 2; i < len(list); i++ {

		clause, ok := ListPairs(list[i].Car())
		if !ok || len(clause) < 2 {
			return nil, list[i].Errorf("case: invalid clause: %v", list[i])
		}

		// (else expr1 expr2...)
		var isElse bool
		if isKeyword(clause[0].Car(), KwElse) {
			if i+1 < len(list) {
				return nil,
					clause[0].Errorf("case: else must be the last clause")
			}
			isElse = true
		}

		choice := &ASTCaseChoice{
			From: clause[0],
		}
		ast.Choices = append(ast.Choices, choice)

		if !isElse {
			// Compare datums: ((datum1 ...) expr1 expr2...)
			datums, ok := ListPairs(clause[0].Car())
			if !ok || len(clause) == 0 {
				return nil, clause[0].Errorf("cond: invalid clause: %v",
					clause[0])
			}

			eqvEnv := env.Copy()
			eqvEnv.PushCaptureFrame(false, FUFrame, 1)
			ast.EqvArgFrame = eqvEnv.PushCaptureFrame(false, FUArgs, 2)

			for _, datum := range datums {
				// (eqv? value datum)
				choice.Datums = append(choice.Datums, datum.Car())
				choice.DatumLocs = append(choice.DatumLocs, datum)
			}
		}

		// Compile expressions.
		for j := 1; j < len(clause); j++ {
			last := j+1 >= len(clause)

			expr, err := p.parseValue(env, clause[j], clause[j].Car(),
				tail && last, captures)
			if err != nil {
				return nil, err
			}
			choice.Exprs = append(choice.Exprs, expr)
		}
	}

	env.PopFrame()

	return ast, nil
}

func (p *Parser) parseAnd(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	ast := &ASTAnd{
		From: list[0],
	}

	for i := 1; i < len(list); i++ {
		expr, err := p.parseValue(env, list[i], list[i].Car(), false, captures)
		if err != nil {
			return nil, err
		}
		ast.Exprs = append(ast.Exprs, expr)
	}

	return ast, nil
}

func (p *Parser) parseOr(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	ast := &ASTOr{
		From: list[0],
	}

	for i := 1; i < len(list); i++ {
		expr, err := p.parseValue(env, list[i], list[i].Car(), false, captures)
		if err != nil {
			return nil, err
		}
		ast.Exprs = append(ast.Exprs, expr)
	}

	return ast, nil
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}

func isIdentifier(value Value) (*Identifier, bool) {
	id, ok := value.(*Identifier)
	return id, ok
}

func isNamedIdentifier(value Value, name string) bool {
	id, ok := isIdentifier(value)
	return ok && id.Name == name
}

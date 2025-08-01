//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"io"
	"sort"

	"github.com/markkurossi/scheme/types"
)

var qqNamedLet, qqGuard, qqGuardRaise, qqUnless Value

func init() {
	qqNamedLet = MustParseSexpr(
		"`(letrec `(`(,,,name `(lambda ,,,,args ,,,,@body))) " +
			"`(,,@init))")
	qqGuard = MustParseSexpr(
		"`(with-exception-handler\n" +
			"`(lambda `(,,,variable) `(cond ,,,@conditions))\n" +
			"`(lambda () ,,@body))")
	qqGuardRaise = MustParseSexpr(
		"`(with-exception-handler\n" +
			"`(lambda `(,,,variable)\n" +
			"  `(cond ,,,@conditions `(else `(raise ,,,,,variable))))\n" +
			"`(lambda () ,,@body))")
	qqUnless = MustParseSexpr(
		"`(if `(not ,,condition)\n" +
			"`(begin ,,@body))")
}

// Parser implements the byte-code compiler.
type Parser struct {
	scm    *Scheme
	source string
}

type export struct {
	from Locator
	id   *Symbol
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
			first = false

			pair, ok := v.(Pair)
			if ok && IsSymbol(pair.Car(), "library") {
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
			}

			// Not a library source.
			library.ExportAll = true
			library.Name = NewPair(NewSymbol("main"), nil)

			// Check for top-level imports.
			if ok && IsSymbol(pair.Car(), "import") {
				_, ok := ListPairs(v)
				if !ok {
					return nil, pair.Errorf("expected (import ...)")
				}
				library.Imports, _ = Cdr(pair, true)
				continue
			}
		}

		var from Point
		locator, ok := v.(Locator)
		if ok {
			from = locator.From()
		} else {
			id, ok := v.(*Symbol)
			if ok {
				from = id.Point
			}
		}
		ast, err := p.parseValue(env, from, v, false, true)
		if err != nil {
			return nil, err
		}
		library.Body.Add(ast)
	}

	if library.ExportAll {
		var exports []string
		for _, item := range library.Body.Items {
			switch ast := item.(type) {
			case *ASTDefine:
				exports = append(exports, ast.Name.Name)

			case *ASTLambda:
				if ast.Define {
					exports = append(exports, ast.Name.Name)
				}
			}
		}
		sort.Strings(exports)
		for i := len(exports) - 1; i >= 0; i-- {
			library.Exports = NewPair(p.scm.Intern(exports[i]), library.Exports)
		}
	}

	return library, nil
}

// parseValue parses the value into AST. XXX we should run the macro
// expansion (and the hard-coded ones too (named let, guard)) before
// calling parseValue. Now we have to detect closures in parseLambda
// and it must also check the guard keyword.
func (p *Parser) parseValue(env *Env, loc Locator, value Value,
	tail, captures bool) (AST, error) {

	switch v := value.(type) {
	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return nil, v.Errorf("unexpected value: %v", v)
		}
		length := len(list)

		sym, ok := v.Car().(*Symbol)
		if ok {
			switch sym.Name {
			case "lambda":
				return p.parseLambda(env, false, 0, list)
			case "define":
				return p.parseDefine(env, list, 0, captures)
			case "define-constant":
				return p.parseDefine(env, list, FlagConst, captures)
			case "if":
				return p.parseIf(env, list, tail, captures)
			case "set!":
				return p.parseSet(env, list, captures)
			case "let":
				return p.parseLet(Let, env, list, tail, captures)
			case "let*":
				return p.parseLet(LetStar, env, list, tail, captures)
			case "letrec":
				return p.parseLet(Letrec, env, list, tail, captures)
			case "pragma":
				return p.parsePragma(env, list)
			case "cond":
				return p.parseCond(env, list, tail, captures)
			case "case":
				return p.parseCase(env, list, tail, captures)
			case "and":
				return p.parseAnd(env, list, tail, captures)
			case "or":
				return p.parseOr(env, list, tail, captures)
			case "define-syntax":
				return p.parseMacro(env, MacroDefine, list)

			case "begin":
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

			case "quote":
				if length != 2 {
					return nil, v.Errorf("invalid quote: %v", v)
				}
				quoted, _ := Unbox(list[1].Car())
				return &ASTConstant{
					From:   loc,
					Quoted: true,
					Value:  quoted,
				}, nil

			case "scheme::apply":
				if length != 3 {
					return nil, v.Errorf("invalid scheme::apply: %v", v)
				}
				return p.parseApply(env, v, tail, captures)

			case "guard":
				return p.parseGuard(env, list, tail, captures)
			case "unless":
				return p.parseUnless(env, list, tail, captures)
			}
		}

		// Function call.

		// Unary inline functions.
		ok, inlineOp, inlineI := p.inlineUnary(env, list)
		if ok {
			ast := &ASTCallUnary{
				From: list[0],
				Op:   inlineOp,
				I:    inlineI,
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
		if tail && length == 1 && false {
			fmt.Printf("parseValue: call, tail=%v\n", tail)
			env.Print()
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

	case *Symbol:
		var sym *Symbol
		binding, ok := env.Lookup(v.Name)
		if !ok {
			sym = p.scm.Intern(v.Name)
		}
		return &ASTSymbol{
			From:    loc,
			Name:    v.Name,
			Binding: binding,
			Global:  sym,
		}, nil

	case Vector:
		// Vector literals must be quoted like list constants.
		return nil, loc.Errorf("invalid syntax: %v", v)

	case Bytevector, Boolean, String, Character, Int, Float, *BigInt, *BigFloat:
		return &ASTConstant{
			From:  loc,
			Value: v,
		}, nil

	case *Number:
		return &ASTConstant{
			From:        loc,
			Value:       v.Value(),
			LexicalType: v.Type(),
		}, nil

	default:
		return nil, loc.Errorf("parseValue: unsupported value: %v(%T)", v, v)
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

var inlineUnaryBinary = map[string]Operand{
	"+": OpAddConst,
	"-": OpSubConst,
	"*": OpMulConst,
}

func (p *Parser) inlineUnary(env *Env, list []Pair) (bool, Operand, int) {
	sym, ok := list[0].Car().(*Symbol)
	if !ok {
		return false, 0, 0
	}

	switch len(list) {
	case 2:
		op, ok := inlineUnary[sym.Name]
		if !ok {
			return false, 0, 0
		}
		return true, op, 0

	case 3:
		op, ok := inlineUnaryBinary[sym.Name]
		if !ok {
			return false, 0, 0
		}
		v, t := Unbox(list[2].Car())
		if !t.IsA(types.InexactInteger) {
			return false, 0, 0
		}
		iv, ok := v.(Int)
		if !ok {
			return false, 0, 0
		}
		return true, op, int(iv)

	default:
		return false, 0, 0
	}
}

var inlineBinary = map[string]Operand{
	"cons": OpCons,
	"+":    OpAdd,
	"-":    OpSub,
	"*":    OpMul,
	"/":    OpDiv,
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
	sym, ok := list[0].Car().(*Symbol)
	if !ok {
		return false, 0
	}
	op, ok := inlineBinary[sym.Name]
	if !ok {
		return false, 0
	}

	return true, op
}

func (p *Parser) parsePragma(env *Env, list []Pair) (AST, error) {
	ast := &ASTPragma{
		From: list[0],
	}
	for i := 1; i < len(list); i++ {
		l := list[i]
		values, ok := ListValues(l.Car())
		if !ok {
			return nil, l.Errorf("invalid pragma: %v", l)
		}
		ast.Directives = append(ast.Directives, values)
	}
	return ast, nil
}

func (p *Parser) parseDefine(env *Env, list []Pair, flags Flags,
	captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("syntax error: %v", list[0])
	}
	// (define name value)
	name, ok := list[1].Car().(*Symbol)
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

	var name *Symbol
	var args Args

	seen := newSeen()

	arg, ok := list[1].Car().(*Symbol)
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
			arg, ok = pair.Car().(*Symbol)
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

			arg, ok = pair.Cdr().(*Symbol)
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
		if idx == 0 {
			if IsSymbol(p.Car(), "lambda") ||
				IsSymbol(p.Car(), "define") ||
				IsSymbol(p.Car(), "guard") {
				lambdas++
				return ErrNext
			}
			cdr, ok := p.Cdr().(Pair)
			if ok && IsSymbol(p.Car(), "let") && Symbolp(cdr.Car()) {
				// Named let captures as the loop is implemented as
				// letrec+lambda.
				lambdas++
				return ErrNext
			}
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

	capture := env.CopyEnvFrames()
	capture.PushCaptureFrame(captures, FUArgs, numArgs)

	var argBindings []*EnvBinding

	for _, arg := range args.Fixed {
		b, err := capture.Define(arg.Name, types.Unspecified)
		if err != nil {
			return nil, err
		}
		argBindings = append(argBindings, b)
	}
	if args.Rest != nil {
		b, err := capture.Define(args.Rest.Name, &types.Type{
			Enum: types.EnumPair,
			Car:  types.Unspecified,
			Cdr:  types.Any,
		})
		if err != nil {
			return nil, err
		}
		argBindings = append(argBindings, b)
	}

	ast := &ASTLambda{
		From:        list[0],
		Name:        name,
		Args:        args,
		ArgBindings: argBindings,
		Env:         capture,
		Captures:    captures,
		Define:      define,
		Flags:       flags,
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
	name, ok := list[1].Car().(*Symbol)
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

func (p *Parser) parseLet(kind LetType, env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("%s: missing bindings or body", kind)
	}
	var idxBindings, idxBody int
	var namedLet bool
	switch list[1].Car().(type) {
	case *Symbol:
		namedLet = true
	}
	if kind == Let && namedLet {
		if len(list) < 4 {
			return nil, list[0].Errorf("named let: missing body")
		}
		idxBindings = 2
		idxBody = 3
	} else {
		namedLet = false
		idxBindings = 1
		idxBody = 2
	}
	bindings, ok := ListPairs(list[idxBindings].Car())
	if !ok {
		return nil, list[idxBindings].Errorf("%s: invalid bindings: %v",
			kind, list[idxBindings].Car())
	}
	if namedLet {
		// (let name ((vn initn)   => (letrec ((name (lambda (vn...)
		//            ...)                             body)))
		//   body)                      (name initn...))

		argsBuilder := new(ListBuilder)
		initBuilder := new(ListBuilder)

		initBuilder.AddPair(DerivePair(list[1], list[1].Car(), nil))

		for _, binding := range bindings {
			def, ok := ListPairs(binding.Car())
			if !ok || len(def) != 2 {
				return nil,
					list[idxBindings].Errorf("named let: invalid init: %v",
						binding)
			}
			argsBuilder.AddPair(DerivePair(def[0], def[0].Car(), nil))
			initBuilder.AddPair(DerivePair(def[1], def[1].Car(), nil))
		}

		qqEnv := NewEvalEnv(nil)
		qqEnv.Set("name", list[1].Car())
		qqEnv.Set("args", argsBuilder.B())
		qqEnv.Set("init", initBuilder.B())
		qqEnv.Set("body", list[idxBody])

		n, err := Eval(qqNamedLet, qqEnv)
		if err != nil {
			return nil, err
		}
		PatchLocation(n, list[0].From())

		namedList, ok := ListPairs(n)
		if !ok {
			panic("named let")
		}
		return p.parseLet(Letrec, env, namedList, tail, true)
	}

	// XXX check if the let inits or body captures and use that as the
	// capture flag.

	letEnv := env.Copy()
	letEnv.PushCaptureFrame(captures, FULet, len(bindings))

	var letBindings []*EnvBinding

	for _, binding := range bindings {
		def, ok := ListPairs(binding.Car())
		if !ok || len(def) != 2 {
			return nil, list[idxBindings].Errorf("%s: invalid init: %v",
				kind, binding)
		}
		name, ok := def[0].Car().(*Symbol)
		if !ok {
			return nil, def[0].Errorf("%s: invalid variable: %v", kind, binding)
		}
		b, err := letEnv.Define(name.Name, types.Unspecified)
		if err != nil {
			return nil, err
		}
		if kind != Letrec {
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
			return nil, list[idxBindings].Errorf("%s: invalid init: %v",
				kind, binding)
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

		switch kind {
		case LetStar:
			letBindings[idx].Disabled = false
		}
	}

	if kind == Let {
		for i := 0; i < len(letBindings); i++ {
			letBindings[i].Disabled = false
		}
	}

	// Compile body.
	for i := idxBody; i < len(list); i++ {
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
		if IsSymbol(clause[0].Car(), "else") {
			if i+1 < len(list) {
				return nil,
					clause[0].Errorf("cond: else must be the last clause")
			}
			isElse = true
			ast.Conclusive = true
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
		if len(clause) > 1 && IsSymbol(clause[1].Car(), "=>") {
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
			env.PushCaptureFrame(false, FUFrame, 1)

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

func (p *Parser) parseCase(env *Env, list []Pair, tail, captures bool) (
	AST, error) {

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
		if IsSymbol(clause[0].Car(), "else") {
			if i+1 < len(list) {
				return nil,
					clause[0].Errorf("case: else must be the last clause")
			}
			isElse = true
			ast.Conclusive = true
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
				v, _ := Unbox(datum.Car())
				choice.Datums = append(choice.Datums, v)
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

func (p *Parser) parseGuard(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	// (guard (variable			=> (with-exception-handler
	//         condClause...)        (lambda (variable)
	//        body)                    (cond condClause...))
	//                               (lambda ()
	//                                 body))
	if len(list) < 3 {
		return nil, list[0].Errorf("guard: missing body")
	}
	clauses, ok := ListPairs(list[1].Car())
	if !ok || len(clauses) < 2 {
		return nil, list[1].Errorf("guard: invalid clauses")
	}
	if !Symbolp(clauses[0].Car()) {
		return nil, clauses[0].Errorf("guard: invalid variable")
	}
	// Check if the clauses do not have else, then add (raise var).
	var qq Value
	last := clauses[len(clauses)-1]
	car, ok := Car(last.Car(), true)
	if ok && IsSymbol(car, "else") {
		qq = qqGuard
	} else {
		qq = qqGuardRaise
	}

	qqEnv := NewEvalEnv(nil)
	qqEnv.Set("variable", clauses[0].Car())
	qqEnv.Set("conditions", clauses[1])
	qqEnv.Set("body", list[2])

	n, err := Eval(qq, qqEnv)
	if err != nil {
		return nil, err
	}
	PatchLocation(n, list[0].From())

	return p.parseValue(env, list[0], n, tail, captures)
}

func (p *Parser) parseUnless(env *Env, list []Pair,
	tail, captures bool) (AST, error) {
	if len(list) < 3 {
		return nil, list[0].Errorf("unless: missing body")
	}

	qqEnv := NewEvalEnv(nil)
	qqEnv.Set("condition", list[1].Car())
	qqEnv.Set("body", list[2])

	n, err := Eval(qqUnless, qqEnv)
	if err != nil {
		return nil, err
	}
	PatchLocation(n, list[0].From())

	return p.parseValue(env, list[0], n, tail, captures)
}

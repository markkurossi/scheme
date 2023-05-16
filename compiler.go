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
	"os"

	"github.com/markkurossi/scheme/types"
)

// Compiler implements the byte-code compiler.
type Compiler struct {
	scm    *Scheme
	source string

	// Library header.
	libraryName Value
	exportAll   bool
	exports     Value
	exported    map[string]*export
	imports     Value

	code      Code
	pcmap     PCMap
	lambdas   []*lambdaCompilation
	nextLabel int
}

type export struct {
	from Locator
	id   *Identifier
}

// Library implements a Scheme compilation unit.
type Library struct {
	Source  string
	Name    Value
	Exports Value
	Imports Value
	Init    Code
	PCMap   PCMap
}

// MapPC maps the program counter value to the source location.
func (m *Library) MapPC(pc int) (source string, line int) {
	source = m.Source

	if false {
		fmt.Printf("Library.MapPC: %v:%v\n", source, pc)
		for idx, pm := range m.PCMap {
			fmt.Printf(" - %v\tPC=%v, Line=%v\n", idx, pm.PC, pm.Line)
		}
		m.Init.Print(os.Stdout)
	}

	line = m.PCMap.MapPC(pc)
	return
}

// PCMap implements mapping from program counter values to source line
// numbers.
type PCMap []PCLine

// MapPC maps the program counter value to the source line number.
func (pcmap PCMap) MapPC(pc int) (line int) {
	for _, pm := range pcmap {
		if pc > pm.PC {
			line = pm.Line
		}
		if pc <= pm.PC {
			break
		}
	}
	return
}

// PCLine maps program counter values to line numbers.
type PCLine struct {
	PC   int
	Line int
}

// Code implements scheme bytecode.
type Code []*Instr

// Print prints the code to standard output.
func (code Code) Print(w io.Writer) {
	for idx, c := range code {
		fmt.Fprintf(w, "%v\t%s\n", idx, c)
	}
}

// NewCompiler creates a new bytecode compiler.
func NewCompiler(scm *Scheme) *Compiler {
	return &Compiler{
		scm:      scm,
		exported: make(map[string]*export),
	}
}

// CompileFile compiles the Scheme file.
func (c *Compiler) CompileFile(file string) (*Library, error) {
	in, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer in.Close()
	return c.Compile(file, in)
}

// Compile compiles the scheme source.
func (c *Compiler) Compile(source string, in io.Reader) (*Library, error) {
	parser := NewParser(source, in)

	c.source = source
	c.code = nil
	c.pcmap = nil
	c.lambdas = nil
	c.nextLabel = 0
	c.scm.Parsing = true

	env := NewEnv()

	// Top-level definitions are executed inside an empty lambda so
	// push the empty argument frame.
	env.PushFrame(TypeEnv, FUArgs, 0)

	first := true

	library := &Library{
		Source: source,
	}

	for {
		v, err := parser.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			}
			break
		}
		c.scm.Parsing = false

		// Check libraries.
		if first {
			pair, ok := v.(Pair)
			if ok && isNamedIdentifier(pair.Car(), "library") {
				list, ok := ListPairs(v)
				if !ok || len(list) < 4 {
					return nil, pair.Errorf("invalid library: %v", v)
				}
				err = c.parseLibraryHeader(list)
				if err != nil {
					return nil, err
				}

				// Compile library body.
				for i := 4; i < len(list); i++ {
					err = c.compileValue(env, list[i], list[i].Car(), false,
						true)
					if err != nil {
						return nil, err
					}
				}

				// Check that the file does not have any trailing garbage
				// after the library specification.
				v, err = parser.Next()
				if err == nil {
					return nil, fmt.Errorf("garbage after library: %v", v)
				}
				if err != io.EOF {
					return nil, err
				}
				break
			} else {
				// Not a library source.
				c.exportAll = true
				c.libraryName = NewPair(&Identifier{
					Name: "main",
				}, nil)
			}
			first = false
		}

		err = c.compileValue(env, Point{}, v, false, true)
		if err != nil {
			return nil, err
		}
	}
	c.addInstr(parser, OpReturn, nil, 0)

	library.PCMap = c.pcmap

	// Compile lambdas.

	var pcmaps []PCMap

	for i := 0; i < len(c.lambdas); i++ {
		lambda := c.lambdas[i]
		pcmapStart := len(c.pcmap)

		// Lambda body starts after the label.
		ofs := len(c.code)
		lambda.Start = ofs + 1

		c.addInstr(nil, OpLabel, nil, lambda.Start)
		for _, ast := range lambda.Body {
			err := ast.Bytecode(c)
			if err != nil {
				return nil, err
			}
		}
		c.addInstr(nil, OpReturn, nil, 0)
		lambda.End = len(c.code)

		pcmap := c.pcmap[pcmapStart:len(c.pcmap)]
		for i := 0; i < len(pcmap); i++ {
			pcmap[i].PC -= lambda.Start
		}
		pcmaps = append(pcmaps, pcmap)
	}

	// Collect label offsets
	labels := make(map[int]int)
	for idx, c := range c.code {
		if c.Op == OpLabel {
			labels[c.I] = idx
		}
	}

	// Patch code offsets.
	for i := 0; i < len(c.code); i++ {
		instr := c.code[i]
		switch instr.Op {
		case OpLambda:
			pcmap := pcmaps[instr.I]
			def := c.lambdas[instr.I]

			var name string
			if def.Name != nil {
				name = def.Name.Name
			}

			instr.I = def.Start
			instr.J = def.End
			instr.V = &LambdaImpl{
				Name:     name,
				Args:     def.Args,
				Return:   def.Body[len(def.Body)-1].Type(),
				Captures: def.Captures,
				Source:   c.source,
				Code:     c.code[def.Start:def.End],
				MaxStack: def.Env.Stats.MaxStack,
				PCMap:    pcmap,
				Body:     def.Body,
			}

		case OpIf, OpIfNot, OpJmp:
			ofs, ok := labels[instr.J]
			if !ok {
				return nil, fmt.Errorf("Label l%v not defined", instr.J)
			}
			instr.I = ofs - i
		}
	}

	// Check that all exported names were defined.
	for k, v := range c.exported {
		if v.id == nil {
			return nil, v.from.Errorf("exported symbol '%s' not defined", k)
		}
	}

	library.Name = c.libraryName
	library.Exports = c.exports
	library.Imports = c.imports
	library.Init = c.code

	return library, nil
}

func (c *Compiler) parseLibraryHeader(list []Pair) error {
	if len(list) < 4 {
		return list[0].Errorf("truncated library header")
	}
	// Name.
	pair, ok := list[1].Car().(Pair)
	if !ok {
		return list[1].Errorf("invalid library name: %v", list[1].Car())
	}
	l, ok := ListPairs(pair)
	if !ok || len(l) == 0 {
		return list[1].Errorf("invalid library name: %v", pair)
	}
	c.libraryName = pair

	// Export.
	l, ok = ListPairs(list[2].Car())
	if !ok || len(l) == 0 || !isNamedIdentifier(l[0].Car(), "export") {
		return list[2].Errorf("expected (export ...)")
	}
	for i := 1; i < len(l); i++ {
		id, ok := isIdentifier(l[i].Car())
		if !ok {
			return l[i].Errorf("invalid export name: %v", l[i])
		}
		c.exported[id.Name] = &export{
			from: l[i],
		}
	}

	// Import.
	pair, ok = list[3].Car().(Pair)
	if !ok {
		return list[3].Errorf("invalid library import: %v", list[3].Car())
	}
	_, ok = ListPairs(list[3].Car())
	if !ok {
		return list[3].Errorf("expected (import ...)")
	}
	c.imports, _ = Cdr(pair, true)

	return nil
}

func (c *Compiler) compileValue(env *Env, loc Locator, value Value,
	tail, captures bool) error {

	ast, err := c.astValue(env, loc, value, tail, captures)
	if err != nil {
		return err
	}

	return ast.Bytecode(c)
}

func (c *Compiler) astValue(env *Env, loc Locator, value Value,
	tail, captures bool) (AST, error) {

	switch v := value.(type) {
	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return nil, v.Errorf("unexpected value: %v", v)
		}
		length := len(list)

		if isKeyword(v.Car(), KwDefine) {
			return c.astDefine(env, list, 0, captures)
		}
		if isKeyword(v.Car(), KwDefineConstant) {
			return c.astDefine(env, list, FlagConst, captures)
		}
		if isKeyword(v.Car(), KwLambda) {
			return c.astLambda(env, false, 0, list)
		}
		if isKeyword(v.Car(), KwSet) {
			return c.astSet(env, list, captures)
		}
		if isKeyword(v.Car(), KwLet) {
			return c.astLet(KwLet, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetStar) {
			return c.astLet(KwLetStar, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetrec) {
			return c.astLet(KwLetrec, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwBegin) {
			seq := &ASTSequence{
				From: loc,
			}
			err := MapPairs(func(idx int, p Pair) error {
				ast, err := c.astValue(env, p, p.Car(),
					tail && idx+1 >= length-1, captures)
				if err != nil {
					return err
				}
				seq.Items = append(seq.Items, ast)
				return nil
			}, v.Cdr())
			if err != nil {
				return nil, err
			}
			return seq, nil
		}
		if isKeyword(v.Car(), KwIf) {
			return c.astIf(env, list, tail, captures)
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
			return c.astApply(env, v, tail, captures)
		}
		if isKeyword(v.Car(), KwCond) {
			return c.astCond(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwCase) {
			return c.astCase(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwAnd) {
			return c.astAnd(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwOr) {
			return c.astOr(env, list, tail, captures)
		}

		// Function call.

		// Unary inline functions.
		ok, inlineOp := c.inlineUnary(env, list)
		if ok {
			ast := &ASTCallUnary{
				From: list[0],
				Op:   inlineOp,
			}
			arg, err := c.astValue(env, list[1], list[1].Car(), false, captures)
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
		ok, inlineOp = c.inlineBinary(env, list)
		if ok {
			ast.Inline = true
			ast.InlineOp = inlineOp
		}

		// Environment for the lambda body when its arguments are
		// evaluated.
		lambdaEnv := env.Copy()

		if !ast.Inline {
			// Compile function.
			a, err := c.astValue(env, list[0], list[0].Car(), false, captures)
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
			a, err := c.astValue(lambdaEnv, list[i], list[i].Car(), false,
				captures)
			if err != nil {
				return nil, err
			}
			ast.Args = append(ast.Args, a)
			ast.ArgLocs = append(ast.ArgLocs, list[i])
		}

		return ast, nil

	case *Identifier:
		binding, _ := env.Lookup(v.Name)
		return &ASTIdentifier{
			From:    loc,
			Name:    v.Name,
			Binding: binding,
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

func (c *Compiler) inlineUnary(env *Env, list []Pair) (bool, Operand) {

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

func (c *Compiler) inlineBinary(env *Env, list []Pair) (bool, Operand) {

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

func (c *Compiler) addCall(from Locator, numArgs int, tail bool) {
	if numArgs >= 0 {
		c.addInstr(from, OpConst, Int(numArgs), 0)
	}
	var i int
	if tail {
		i = 1
	}
	c.addInstr(from, OpCall, nil, i)
}

func (c *Compiler) addPushS(from Locator, size int, capture bool) {
	var op Operand
	if capture {
		op = OpPushE
	} else {
		op = OpPushS
	}
	c.addInstr(from, op, nil, size)
}

func (c *Compiler) addPopS(from Locator, size int, capture bool) {
	var op Operand
	if capture {
		op = OpPopE
	} else {
		op = OpPopS
	}
	instr := c.addInstr(from, op, nil, size)
	if !capture {
		instr.J = 1
	}
}

func (c *Compiler) addInstr(from Locator, op Operand, v Value, i int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
	}
	if from != nil {
		p := from.From()
		if len(c.pcmap) == 0 || c.pcmap[len(c.pcmap)-1].Line != p.Line {
			c.pcmap = append(c.pcmap, PCLine{
				PC:   len(c.code),
				Line: p.Line,
			})
		}
	}
	c.code = append(c.code, instr)
	return instr
}

func (c *Compiler) addLabel(l *Instr) {
	c.code = append(c.code, l)
}

func (c *Compiler) newLabel() *Instr {
	c.nextLabel++
	return &Instr{
		Op: OpLabel,
		I:  c.nextLabel - 1,
	}
}

func (c *Compiler) astDefine(env *Env, list []Pair, flags Flags,
	captures bool) (AST, error) {

	if len(list) < 3 {
		return nil, list[0].Errorf("syntax error: %v", list[0])
	}
	// (define name value)
	name, ok := isIdentifier(list[1].Car())
	if ok {
		ast, err := c.astValue(env, list[2], list[2].Car(), false, captures)
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
	return c.astLambda(env, true, flags, list)
}

func (c *Compiler) define(loc Locator, env *Env, name *Identifier,
	flags Flags) error {

	export, ok := c.exported[name.Name]
	if c.exportAll || ok {
		// XXX Defined global symbol.
	} else {
		// XXX Define library symbol.
		// fmt.Printf("XXX define local symbol %v\n", name)
	}
	if ok {
		export.id = name
	}
	c.exports = NewPair(name, c.exports)

	instr := c.addInstr(loc, OpDefine, nil, int(flags))
	instr.Sym = c.scm.Intern(name.Name)

	return nil
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

func (c *Compiler) astLambda(env *Env, define bool, flags Flags,
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
		_, err := capture.Define(arg.Name)
		if err != nil {
			return nil, err
		}
	}
	if args.Rest != nil {
		_, err := capture.Define(args.Rest.Name)
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
		a, err := c.astValue(capture, list[i], list[i].Car(), i+1 >= len(list),
			captures)
		if err != nil {
			return nil, err
		}
		ast.Body = append(ast.Body, a)
	}

	return ast, nil
}

func (c *Compiler) astSet(env *Env, list []Pair, captures bool) (AST, error) {
	// (set! name value)
	if len(list) != 3 {
		return nil, list[0].Errorf("syntax error: %v", list[0])
	}
	name, ok := isIdentifier(list[1].Car())
	if !ok {
		return nil, list[1].Errorf("set!: expected variable name: %v",
			list[1].Car())
	}
	ast, err := c.astValue(env, list[2], list[2].Car(), false, captures)
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

func (c *Compiler) astLet(kind Keyword, env *Env, list []Pair,
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
		b, err := letEnv.Define(name.Name)
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

		initAst, err := c.astValue(letEnv, def[1], def[1].Car(), false,
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
		bodyAst, err := c.astValue(letEnv, list[i], list[i].Car(),
			tail && i+1 >= len(list), captures)
		if err != nil {
			return nil, err
		}
		ast.Body = append(ast.Body, bodyAst)
	}

	return ast, nil
}

func (c *Compiler) astIf(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	if len(list) < 3 || len(list) > 4 {
		return nil, list[0].Errorf("if: syntax error")
	}

	ast := &ASTIf{
		From: list[0],
	}
	a, err := c.astValue(env, list[1], list[1].Car(), false, captures)
	if err != nil {
		return nil, err
	}
	ast.Cond = a

	a, err = c.astValue(env, list[2], list[2].Car(), tail, captures)
	if err != nil {
		return nil, err
	}
	ast.True = a

	if len(list) == 4 {
		a, err = c.astValue(env, list[3], list[3].Car(), tail, captures)
		if err != nil {
			return nil, err
		}
		ast.False = a
	}

	return ast, nil
}

func (c *Compiler) astApply(env *Env, pair Pair,
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
	lambdaAST, err := c.astValue(env, pair, f, false, captures)
	if err != nil {
		return nil, err
	}

	// Create a call frame.
	env.PushFrame(TypeStack, FUFrame, 1)

	// Compile arguments.
	argsAST, err := c.astValue(env, pair, args, false, captures)
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

func (c *Compiler) astCond(env *Env, list []Pair,
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
			condAST, err := c.astValue(env, clause[0], clause[0].Car(), false,
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
			funcAST, err := c.astValue(env, clause[2], clause[2].Car(), false,
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
				expr, err := c.astValue(env, clause[j], clause[j].Car(),
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

func (c *Compiler) astCase(env *Env, list []Pair,
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
	expr, err := c.astValue(env, list[1], list[1].Car(), false, captures)
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

			expr, err := c.astValue(env, clause[j], clause[j].Car(),
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

func (c *Compiler) astAnd(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	ast := &ASTAnd{
		From: list[0],
	}

	for i := 1; i < len(list); i++ {
		expr, err := c.astValue(env, list[i], list[i].Car(), false, captures)
		if err != nil {
			return nil, err
		}
		ast.Exprs = append(ast.Exprs, expr)
	}

	return ast, nil
}

func (c *Compiler) astOr(env *Env, list []Pair,
	tail, captures bool) (AST, error) {

	ast := &ASTOr{
		From: list[0],
	}

	for i := 1; i < len(list); i++ {
		expr, err := c.astValue(env, list[i], list[i].Car(), false, captures)
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

type lambdaCompilation struct {
	Start    int
	End      int
	Name     *Identifier
	Args     Args
	Body     []AST
	Env      *Env
	MaxStack int
	Captures bool
}

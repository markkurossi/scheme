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
	lambdas   []*lambdaBody
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
		m.Init.Print()
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
func (code Code) Print() {
	for idx, c := range code {
		fmt.Printf("%v\t%s\n", idx, c)
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
		for idx := 0; idx < len(lambda.Body); idx++ {
			err := c.compileValue(lambda.Env, lambda.Body[idx],
				lambda.Body[idx].Car(), idx+1 >= len(lambda.Body),
				lambda.Captures)
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
			instr.I = def.Start
			instr.J = def.End
			instr.V = &Lambda{
				Args:     def.Args,
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

	switch v := value.(type) {
	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return v.Errorf("unexpected value: %v", v)
		}
		length := len(list)

		if isKeyword(v.Car(), KwDefine) {
			return c.compileDefine(env, list, captures)
		}
		if isKeyword(v.Car(), KwLambda) {
			return c.compileLambda(env, false, list)
		}
		if isKeyword(v.Car(), KwSet) {
			return c.compileSet(env, list, captures)
		}
		if isKeyword(v.Car(), KwLet) {
			return c.compileLet(KwLet, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetStar) {
			return c.compileLet(KwLetStar, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwLetrec) {
			return c.compileLet(KwLetrec, env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwBegin) {
			err := MapPairs(func(idx int, p Pair) error {
				err := c.compileValue(env, p, p.Car(),
					tail && idx+1 >= length-1, captures)
				return err
			}, v.Cdr())
			return err
		}
		if isKeyword(v.Car(), KwIf) {
			return c.compileIf(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwQuote) {
			if length != 2 {
				return v.Errorf("invalid quote: %v", v)
			}
			quoted, ok := Car(v.Cdr(), true)
			if !ok {
				return v.Errorf("invalid quote: %v", v)
			}
			c.addInstr(v, OpConst, quoted, 0)
			return nil
		}
		if isKeyword(v.Car(), KwSchemeApply) {
			if length != 3 {
				return v.Errorf("invalid scheme::apply: %v", v)
			}
			return c.compileApply(env, v, tail, captures)
		}
		if isKeyword(v.Car(), KwCond) {
			return c.compileCond(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwCase) {
			return c.compileCase(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwAnd) {
			return c.compileAnd(env, list, tail, captures)
		}
		if isKeyword(v.Car(), KwOr) {
			return c.compileOr(env, list, tail, captures)
		}

		// Function call.

		// Compile function.
		err := c.compileValue(env, v, v.Car(), false, captures)
		if err != nil {
			return err
		}

		// Environment for the lambda body when its arguments are
		// evaluated.
		lambdaEnv := env.Copy()

		// Create call frame.
		c.addInstr(v, OpPushF, nil, 0)
		lambdaEnv.PushFrame(TypeStack, FUFrame, 1)

		// Push argument scope.
		argFrame := lambdaEnv.PushFrame(TypeStack, FUArgs, length-1)
		c.addInstr(v, OpPushS, nil, length-1)

		// Evaluate arguments.
		li := v.Cdr()
		for j := 0; li != nil; j++ {
			pair, ok := li.(Pair)
			if !ok {
				return v.Errorf("invalid list: %v", li)
			}
			err := c.compileValue(lambdaEnv, pair, pair.Car(), false, captures)
			if err != nil {
				return err
			}
			li = pair.Cdr()

			// XXX reference stack from fp
			instr := c.addInstr(pair, OpLocalSet, nil, argFrame.Index)
			instr.J = j
		}

		c.addCall(nil, length-1, tail)

		return nil

	case *Identifier:
		b, ok := env.Lookup(v.Name)
		if ok {
			if b.Frame.Type == TypeStack {
				c.addInstr(v.Point, OpLocal, nil, b.Frame.Index+b.Index)
			} else {
				instr := c.addInstr(v.Point, OpEnv, nil, b.Frame.Index)
				instr.J = b.Index
			}
		} else {
			instr := c.addInstr(v.Point, OpGlobal, nil, 0)
			instr.Sym = c.scm.Intern(v.Name)
		}

	case Keyword:
		return loc.Errorf("unexpected keyword: %s", v)

	case Vector:
		// Vector literals must be quoted like list constants.
		return loc.Errorf("invalid syntax: %v", v)

	case ByteVector, Boolean, String, Character, Int, *BigInt:
		c.addInstr(nil, OpConst, v, 0)

	default:
		return fmt.Errorf("compileValue: unsupported value: %v(%T)", v, v)
	}
	return nil
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
	return c.addInstr2(from, op, v, i, 0)
}

func (c *Compiler) addInstr2(from Locator, op Operand, v Value, i, j int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
		J:  j,
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

func (c *Compiler) compileDefine(env *Env, list []Pair, captures bool) error {
	if len(list) < 3 {
		return list[0].Errorf("syntax error: %v", list[0])
	}
	// (define name value)
	name, ok := isIdentifier(list[1].Car())
	if ok {
		err := c.compileValue(env, list[2], list[2].Car(), false, captures)
		if err != nil {
			return err
		}
		return c.define(list[1], env, name)
	}

	// (define (name args?) body)
	return c.compileLambda(env, true, list)
}

func (c *Compiler) define(loc Locator, env *Env, name *Identifier) error {
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

	instr := c.addInstr(loc, OpDefine, nil, 0)
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

func (c *Compiler) compileLambda(env *Env, define bool, list []Pair) error {

	// (define (name args?) body)
	// (lambda (args?) body)
	// (lambda args body)
	if len(list) < 3 {
		return list[0].Errorf("missing lambda body: %v", list[0])
	}

	var name *Identifier
	var args Args

	seen := newSeen()

	arg, ok := isIdentifier(list[1].Car())
	if ok {
		if define {
			return list[1].Errorf("invalid define: %v", list[0])
		}
		err := seen.add(arg.Name)
		if err != nil {
			return list[1].Errorf("%v", err)
		}
		args.Rest = arg
	} else {
		var pair Pair
		if list[1].Car() != nil {
			pair, ok = list[1].Car().(Pair)
			if !ok {
				return list[0].Errorf("invalid arguments: %v", list[1].Car())
			}
		}
		for pair != nil {
			arg, ok = isIdentifier(pair.Car())
			if !ok {
				return pair.Errorf("invalid argument: %v", pair.Car())
			}
			if define && name == nil {
				name = arg
			} else {
				err := seen.add(arg.Name)
				if err != nil {
					return pair.Errorf("%v", err)
				}
				args.Fixed = append(args.Fixed, arg)
			}

			arg, ok = isIdentifier(pair.Cdr())
			if ok {
				// Rest arguments.
				err := seen.add(arg.Name)
				if err != nil {
					return fmt.Errorf("%s: %v", pair.To(), err)
				}
				args.Rest = arg
				break
			}
			if pair.Cdr() == nil {
				pair = nil
			} else {
				next, ok := pair.Cdr().(Pair)
				if !ok {
					return pair.Errorf("invalid argument: %v", pair)
				}
				pair = next
			}
		}
	}
	args.Init()

	if define && name == nil {
		return list[0].Errorf("define: name not defined: %v", list[0])
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
			return err
		}
	}
	if args.Rest != nil {
		_, err := capture.Define(args.Rest.Name)
		if err != nil {
			return err
		}
	}

	c.addInstr(list[0], OpLambda, nil, len(c.lambdas))
	c.lambdas = append(c.lambdas, &lambdaBody{
		Args:     args,
		Body:     list[2:],
		Env:      capture,
		Captures: captures,
	})
	if define {
		return c.define(list[0], env, name)
	}

	return nil
}

func (c *Compiler) compileSet(env *Env, list []Pair, captures bool) error {
	// (set! name value)
	if len(list) != 3 {
		return list[0].Errorf("syntax error: %v", list[0])
	}
	name, ok := isIdentifier(list[1].Car())
	if !ok {
		return list[1].Errorf("set!: expected variable name: %v", list[1].Car())
	}
	err := c.compileValue(env, list[2], list[2].Car(), false, captures)
	if err != nil {
		return err
	}

	nameSym := c.scm.Intern(name.Name)
	b, ok := env.Lookup(name.Name)
	if ok {
		if b.Frame.Type == TypeStack {
			c.addInstr(nil, OpLocalSet, nil, b.Frame.Index+b.Index)
		} else {
			instr := c.addInstr(nil, OpEnvSet, nil, b.Frame.Index)
			instr.J = b.Index
		}
	} else {
		instr := c.addInstr(nil, OpGlobalSet, nil, 0)
		instr.Sym = nameSym
	}

	return nil
}

func (c *Compiler) compileLet(kind Keyword, env *Env, list []Pair,
	tail, captures bool) error {

	if len(list) < 3 {
		return list[0].Errorf("%s: missing bindings or body", kind)
	}
	bindings, ok := ListPairs(list[1].Car())
	if !ok {
		return list[1].Errorf("%s: invalid bindings: %v", kind, list[1].Car())
	}

	// XXX check if the let inits or body captures and use that as the
	// capture flag.

	letEnv := env.Copy()
	letEnv.PushCaptureFrame(captures, FULet, len(bindings))

	c.addPushS(list[0], len(bindings), captures)

	var letBindings []*EnvBinding

	for _, binding := range bindings {
		def, ok := ListPairs(binding.Car())
		if !ok || len(def) != 2 {
			return list[1].Errorf("%s: invalid init: %v", kind, binding)
		}
		name, ok := def[0].Car().(*Identifier)
		if !ok {
			return def[0].Errorf("%s: invalid variable: %v", kind, binding)
		}
		b, err := letEnv.Define(name.Name)
		if err != nil {
			return err
		}
		if kind != KwLetrec {
			b.Disabled = true
		}
		letBindings = append(letBindings, b)
	}

	for idx, binding := range bindings {
		def, ok := ListPairs(binding.Car())
		if !ok || len(def) != 2 {
			return list[1].Errorf("%s: invalid init: %v", kind, binding)
		}

		// Compile init value.
		err := c.compileValue(letEnv, def[1], def[1].Car(), false, captures)
		if err != nil {
			return err
		}

		b := letBindings[idx]

		if b.Frame.Type == TypeStack {
			c.addInstr(def[1], OpLocalSet, nil, b.Frame.Index+b.Index)
		} else {
			instr := c.addInstr(def[1], OpEnvSet, nil, b.Frame.Index)
			instr.J = b.Index
		}

		if kind == KwLetStar {
			b.Disabled = false
		}
	}

	if kind == KwLet {
		for i := 0; i < len(letBindings); i++ {
			letBindings[i].Disabled = false
		}
	}

	// fmt.Printf("compileLet: body env:\n")
	// letEnv.Print()

	// Compile body.
	for i := 2; i < len(list); i++ {
		err := c.compileValue(letEnv, list[i], list[i].Car(),
			tail && i+1 >= len(list), captures)
		if err != nil {
			return err
		}
	}

	if !tail {
		c.addPopS(nil, len(bindings), captures)
	}

	return nil
}

func (c *Compiler) compileIf(env *Env, list []Pair, tail, captures bool) error {
	if len(list) < 3 || len(list) > 4 {
		return list[0].Errorf("if: syntax error")
	}

	labelFalse := c.newLabel()
	labelEnd := c.newLabel()

	err := c.compileValue(env, list[1], list[1].Car(), false, captures)
	if err != nil {
		return err
	}
	if len(list) == 3 {
		// (if cond t)
		instr := c.addInstr(list[0], OpIfNot, nil, 0)
		instr.J = labelEnd.I

		err = c.compileValue(env, list[2], list[2].Car(), tail, captures)
		if err != nil {
			return err
		}
	} else {
		// (if cond t f)
		instr := c.addInstr(list[0], OpIfNot, nil, 0)
		instr.J = labelFalse.I

		err = c.compileValue(env, list[2], list[2].Car(), tail, captures)
		if err != nil {
			return err
		}
		instr = c.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I

		c.addLabel(labelFalse)
		err = c.compileValue(env, list[3], list[3].Car(), tail, captures)
		if err != nil {
			return err
		}
	}

	c.addLabel(labelEnd)

	return nil
}

func (c *Compiler) compileApply(env *Env, pair Pair,
	tail, captures bool) error {

	f, ok := Car(pair.Cdr(), true)
	if !ok {
		return pair.Errorf("scheme::apply: invalid list: %v", pair)
	}
	args, ok := Car(Cdr(pair.Cdr(), true))
	if !ok {
		return pair.Errorf("scheme::apply: invalid list: %v", pair)
	}

	// Compile function.
	err := c.compileValue(env, pair, f, false, captures)
	if err != nil {
		return err
	}

	// Create a call frame.
	c.addInstr(nil, OpPushF, nil, 0)
	env.PushFrame(TypeStack, FUFrame, 1)

	// Compile arguments.
	err = c.compileValue(env, pair, args, false, captures)
	if err != nil {
		return err
	}

	// Push apply scope.
	c.addInstr(nil, OpPushA, nil, 0)

	// Pop call frame.
	env.PopFrame()

	c.addCall(nil, -1, tail)

	return nil
}

func (c *Compiler) compileCond(env *Env, list []Pair,
	tail, captures bool) error {

	if len(list) < 2 {
		return list[0].Errorf("cond: no clauses")
	}
	labelEnd := c.newLabel()

	var labelClause *Instr

	for i := 1; i < len(list); i++ {
		if labelClause != nil {
			c.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(list) {
			next = c.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		clause, ok := ListPairs(list[i].Car())
		if !ok || len(clause) < 1 {
			return list[i].Errorf("cond: invalid clause: %v", list[i])
		}

		var isElse bool
		if isKeyword(clause[0].Car(), KwElse) {
			if i+1 < len(list) {
				return clause[0].Errorf("cond: else must be the last clause")
			}
			isElse = true
		}

		if !isElse {
			// Compile condition.
			err := c.compileValue(env, clause[0], clause[0].Car(), false,
				captures)
			if err != nil {
				return err
			}
			instr := c.addInstr(nil, OpIfNot, nil, 0)
			instr.J = next.I
		}
		// cond => func
		if len(clause) > 1 && isKeyword(clause[1].Car(), KwImplies) {
			if len(clause) != 3 {
				return clause[0].Errorf("cond: invalid => clause")
			}

			// Push value scope.
			valueFrame := env.PushCaptureFrame(false, FUValue, 1)
			c.addInstr(clause[2], OpPushS, nil, 1)

			// Save value
			c.addInstr(clause[2], OpLocalSet, nil, valueFrame.Index)

			// Compile function.
			err := c.compileValue(env, clause[2], clause[2].Car(), false,
				captures)
			if err != nil {
				return err
			}

			// Create call frame.
			c.addInstr(clause[2], OpPushF, nil, 0)
			env.PushCaptureFrame(captures, FUFrame, 1)

			// Push argument scope.
			argsFrame := env.PushCaptureFrame(false, FUArgs, 1)
			c.addInstr(clause[2], OpPushS, nil, 1)

			// Set argument.
			c.addInstr(clause[2], OpLocal, nil, valueFrame.Index)
			c.addInstr(clause[2], OpLocalSet, nil, argsFrame.Index)

			env.PopFrame() // Argument
			env.PopFrame() // Call
			env.PopFrame() // Value

			c.addCall(nil, 1, tail)
			if !tail {
				// Pop value scope.
				c.addPopS(clause[2], 1, captures)
			}
		} else {
			// Compile expressions.
			for j := 1; j < len(clause); j++ {
				last := j+1 >= len(clause)
				err := c.compileValue(env, clause[j], clause[j].Car(),
					tail && last, captures)
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

func (c *Compiler) compileCase(env *Env, list []Pair,
	tail, captures bool) error {

	if len(list) < 3 {
		return list[0].Errorf("case: key or clauses")
	}
	labelEnd := c.newLabel()

	// Push value scope.
	valueFrame := env.PushCaptureFrame(false, FUValue, 1)
	c.addInstr(list[1], OpPushS, nil, 1)

	// Compile key.
	err := c.compileValue(env, list[1], list[1].Car(), false, captures)
	if err != nil {
		return err
	}

	// Save value.
	c.addInstr(list[1], OpLocalSet, nil, valueFrame.Index)

	// Compile clauses

	var labelClause *Instr

	for i := 2; i < len(list); i++ {
		if labelClause != nil {
			c.addLabel(labelClause)
		}

		var next *Instr
		if i+1 < len(list) {
			next = c.newLabel()
			labelClause = next
		} else {
			next = labelEnd
		}

		clause, ok := ListPairs(list[i].Car())
		if !ok || len(clause) < 2 {
			return list[i].Errorf("case: invalid clause: %v", list[i])
		}

		// (else expr1 expr2...)
		var isElse bool
		if isKeyword(clause[0].Car(), KwElse) {
			if i+1 < len(list) {
				return clause[0].Errorf("case: else must be the last clause")
			}
			isElse = true
		}

		var labelExprs *Instr
		if !isElse {
			labelExprs = c.newLabel()

			// Compare datums: ((datum1 ...) expr1 expr2...)
			datums, ok := ListPairs(clause[0].Car())
			if !ok || len(clause) == 0 {
				return clause[0].Errorf("cond: invalid clause: %v", clause[0])
			}
			for _, datum := range datums {
				// (eqv? value datum)
				eqvEnv := env.Copy()

				instr := c.addInstr(datum, OpGlobal, nil, 0)
				instr.Sym = c.scm.Intern("eqv?")

				c.addInstr(datum, OpPushF, nil, 0)
				eqvEnv.PushCaptureFrame(false, FUFrame, 1)

				c.addInstr(datum, OpPushS, nil, 2)
				eqvArgFrame := eqvEnv.PushCaptureFrame(false, FUArgs, 2)

				c.addInstr(datum, OpLocal, nil, valueFrame.Index)
				instr = c.addInstr(datum, OpLocalSet, nil, eqvArgFrame.Index)
				instr.J = 0

				c.addInstr(datum, OpConst, datum.Car(), 0)
				instr = c.addInstr(datum, OpLocalSet, nil, eqvArgFrame.Index)
				instr.J = 1

				c.addCall(datum, 2, false)

				instr = c.addInstr(datum, OpIf, nil, 0)
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
		for j := 1; j < len(clause); j++ {
			last := j+1 >= len(clause)
			err := c.compileValue(env, clause[j], clause[j].Car(),
				tail && last, captures)
			if err != nil {
				return err
			}
		}

		// Jump to end.
		instr := c.addInstr(nil, OpJmp, nil, 0)
		instr.J = labelEnd.I
	}

	c.addLabel(labelEnd)

	if !tail {
		// Pop value scope.
		c.addPopS(nil, 1, captures)
	}
	env.PopFrame()

	return nil
}

func (c *Compiler) compileAnd(env *Env, list []Pair,
	tail, captures bool) error {

	if len(list) == 1 {
		c.addInstr(nil, OpConst, Boolean(true), 0)
		return nil
	}

	labelEnd := c.newLabel()
	for i := 1; i < len(list)-1; i++ {
		err := c.compileValue(env, list[i], list[i].Car(), false, captures)
		if err != nil {
			return err
		}
		instr := c.addInstr(nil, OpIfNot, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	last := list[len(list)-1]
	err := c.compileValue(env, last, last.Car(), tail, captures)
	if err != nil {
		return err
	}

	c.addLabel(labelEnd)

	return nil
}

func (c *Compiler) compileOr(env *Env, list []Pair, tail, captures bool) error {
	if len(list) == 1 {
		c.addInstr(nil, OpConst, Boolean(false), 0)
		return nil
	}
	labelEnd := c.newLabel()
	for i := 1; i < len(list)-1; i++ {
		err := c.compileValue(env, list[i], list[i].Car(), false, captures)
		if err != nil {
			return err
		}
		instr := c.addInstr(nil, OpIf, nil, 0)
		instr.J = labelEnd.I
	}

	// Last expression.
	last := list[len(list)-1]
	err := c.compileValue(env, last, last.Car(), tail, captures)
	if err != nil {
		return err
	}

	c.addLabel(labelEnd)

	return nil
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

// LambdaBody defines the lambda body and its location in the compiled
// bytecode.
type lambdaBody struct {
	Start    int
	End      int
	Args     Args
	Body     []Pair
	Env      *Env
	MaxStack int
	Captures bool
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"os"
	"sort"
)

// CompileFile compiles the Scheme file.
func (scm *Scheme) CompileFile(file string) (Code, error) {
	in, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer in.Close()
	return scm.Compile(file, in)
}

// Compile compiles the scheme source.
func (scm *Scheme) Compile(source string, in io.Reader) (Code, error) {
	parser := NewParser(source, in)

	scm.compiled = nil
	scm.env = NewEnv()
	scm.lambdas = nil
	scm.Parsing = true

	for {
		v, err := parser.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			}
			break
		}
		scm.Parsing = false

		err = scm.compileValue(v, false)
		if err != nil {
			return nil, err
		}
	}
	scm.addInstr(OpHalt, nil, 0)

	// Compile lambdas.
	for i := 0; i < len(scm.lambdas); i++ {
		lambda := scm.lambdas[i]

		scm.env = lambda.Env

		// Lambda body starts after the label.
		ofs := len(scm.compiled)
		lambda.Start = ofs + 1

		length, ok := ListLength(lambda.Body)
		if !ok {
			return nil, fmt.Errorf("invalid lambda body")
		}

		scm.addInstr(OpLabel, nil, lambda.Start)
		err := Map(func(idx int, v Value) error {
			return scm.compileValue(v, idx+1 >= length)
		}, lambda.Body)
		if err != nil {
			return nil, err
		}
		scm.addInstr(OpReturn, nil, 0)
		lambda.End = len(scm.compiled)

		scm.env = nil
	}

	// Collect label offsets
	labels := make(map[int]int)
	for idx, c := range scm.compiled {
		if c.Op == OpLabel {
			labels[c.I] = idx
		}
	}

	// Patch code offsets.
	for i := 0; i < len(scm.compiled); i++ {
		instr := scm.compiled[i]
		switch instr.Op {
		case OpLambda:
			def := scm.lambdas[instr.I]
			instr.I = def.Start
			instr.J = def.End
			instr.V = &Lambda{
				MinArgs: len(def.Args),
				MaxArgs: len(def.Args),
				Args:    def.Args,
				Code:    scm.compiled[def.Start:def.End],
				Body:    def.Body,
				Capture: def.Env.Depth() - 1,
			}

		case OpIf, OpJmp:
			ofs, ok := labels[instr.J]
			if !ok {
				return nil, fmt.Errorf("Label l%v not defined", instr.J)
			}
			instr.I = ofs - i
		}
	}

	return scm.compiled, nil
}

func (scm *Scheme) compileValue(value Value, tail bool) error {
	switch v := value.(type) {
	case Pair:
		length, ok := ListLength(v)
		if !ok {
			return fmt.Errorf("compile value: %v", v)
		}
		if isKeyword(v.Car(), KwDefine) {
			return scm.compileDefine(v, length)
		}
		if isKeyword(v.Car(), KwLambda) {
			return scm.compileLambda(false, v, length)
		}
		if isKeyword(v.Car(), KwSet) {
			return scm.compileSet(v, length)
		}
		if isKeyword(v.Car(), KwBegin) {
			return Map(func(idx int, v Value) error {
				return scm.compileValue(v, tail && idx+1 >= length-1)
			}, v.Cdr())
		}
		if isKeyword(v.Car(), KwIf) {
			return scm.compileIf(v, length, tail)
		}
		if isKeyword(v.Car(), KwQuote) {
			if length != 2 {
				return fmt.Errorf("invalid quote: %v", v)
			}
			quoted, ok := Car(v.Cdr(), true)
			if !ok {
				return fmt.Errorf("invalid quote: %v", v)
			}
			scm.addInstr(OpConst, quoted, 0)
			return nil
		}

		// Function call.

		// Compile function.
		err := scm.compileValue(v.Car(), false)
		if err != nil {
			return err
		}

		// Create a call frame.
		scm.addInstr(OpPushF, scm.accu, 0)
		scm.env.PushFrame()

		// Push argument scope.
		scm.addInstr(OpPushS, nil, length-1)
		scm.env.PushFrame()

		// Evaluate arguments.
		li := v.Cdr()
		for j := 0; li != nil; j++ {
			pair, ok := li.(Pair)
			if !ok {
				return fmt.Errorf("invalid list: %v", li)
			}
			err := scm.compileValue(pair.Car(), false)
			if err != nil {
				return err
			}
			li = pair.Cdr()
			instr := scm.addInstr(OpLocalSet, nil, scm.env.Depth()-1)
			instr.J = j
		}

		// Pop argument scope.
		scm.env.PopFrame()

		// Pop call frame.
		scm.env.PopFrame()

		if tail {
			scm.addInstr(OpCall, nil, 1)
		} else {
			scm.addInstr(OpCall, nil, 0)
		}

		return nil

	case *Identifier:
		b, ok := scm.env.Lookup(v.Name)
		if ok {
			instr := scm.addInstr(OpLocal, nil, b.Frame)
			instr.J = b.Index
		} else {
			instr := scm.addInstr(OpGlobal, nil, 0)
			instr.Sym = scm.Intern(v.Name)
		}

	case Keyword:
		return fmt.Errorf("unexpected keyword: %s", v)

	case Boolean, String, Character, Number:
		scm.addInstr(OpConst, v, 0)

	default:
		return fmt.Errorf("compile value: %v(%T)", v, v)
	}
	return nil
}

func (scm *Scheme) addInstr(op Operand, v Value, i int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
	}
	scm.compiled = append(scm.compiled, instr)
	return instr
}

func (scm *Scheme) addLabel(l *Instr) {
	scm.compiled = append(scm.compiled, l)
}

func (scm *Scheme) newLabel() *Instr {
	scm.nextLabel++
	return &Instr{
		Op: OpLabel,
		I:  scm.nextLabel - 1,
	}
}

func (scm *Scheme) compileDefine(pair Pair, length int) error {
	// (define name value)
	name, _, ok := isIdentifier(Car(Cdr(pair, true)))
	if ok {
		if length != 3 {
			return fmt.Errorf("syntax error: %v", pair)
		}
		v, ok := Car(Cdr(Cdr(pair, true)))
		if !ok {
			return fmt.Errorf("syntax error: %v", pair)
		}
		err := scm.compileValue(v, false)
		if err != nil {
			return err
		}
		return scm.define(name.Name)
	}

	// (define (name args?) body)
	return scm.compileLambda(true, pair, length)
}

func (scm *Scheme) define(name string) error {
	nameSym := scm.Intern(name)
	_, ok := scm.env.Lookup(name)
	if ok || nameSym.Global != nil {
		return fmt.Errorf("symbol '%v' already defined", name)
	}

	if scm.env.Empty() {
		instr := scm.addInstr(OpDefine, nil, 0)
		instr.Sym = nameSym
	} else {
		b, err := scm.env.Define(name)
		if err != nil {
			return err
		}
		instr := scm.addInstr(OpLocalSet, nil, b.Frame)
		instr.J = b.Index
	}
	return nil
}

func (scm *Scheme) compileLambda(define bool, pair Pair, length int) error {
	// (define (name args?) body)
	// (lambda (args?) body)
	lst, ok := Car(Cdr(pair, true))
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}
	_, ok = ListLength(lst)
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}

	var name *Identifier
	var args []*Identifier

	err := Map(func(idx int, v Value) error {
		id, ok := v.(*Identifier)
		if !ok {
			return fmt.Errorf("lambda: arguments must be identifiers")
		}
		if define && name == nil {
			name = id
		} else {
			args = append(args, id)
		}
		return nil
	}, lst)
	if err != nil {
		return err
	}
	if define && name == nil {
		return fmt.Errorf("function name not define")
	}

	lst, ok = Cdr(Cdr(pair, true))
	if !ok {
		return fmt.Errorf("invalid function body")
	}

	var body Pair
	body, ok = lst.(Pair)
	if !ok {
		return fmt.Errorf("invalid function body")
	}

	// The environment (below) is converted to lambda capture:
	//
	//     0: let-frame-m           0: let-frame-m
	//        ...                      ...
	//   n-1: let-frame-0         n-2: let-frame-0
	//     n: ctx-function-args   n-1: ctx-function-args
	//							    n: lambda-args
	//
	// And the final lambda environment (scope) is as follows (the
	// lambda args are pushed to the top of the environment):
	//
	//     0: lambda-args
	//     1: let-frame-m
	//        ...
	//   n-1: let-frame-0
	//     n: ctx-function-args

	env := NewEnv()

	env.PushFrame()
	for _, arg := range args {
		_, err = env.Define(arg.Name)
		if err != nil {
			return err
		}
	}
	env.Push(scm.env)
	env.ShiftDown()

	scm.addInstr(OpLambda, nil, len(scm.lambdas))
	scm.lambdas = append(scm.lambdas, &LambdaBody{
		Args: args,
		Body: body,
		Env:  env,
	})
	if define {
		return scm.define(name.Name)
	}

	return nil
}

func (scm *Scheme) compileSet(pair Pair, length int) error {
	// (set! name value)
	if length != 3 {
		return fmt.Errorf("syntax error: %v", pair)
	}
	name, nameV, ok := isIdentifier(Car(Cdr(pair, true)))
	if !ok {
		return fmt.Errorf("set!: expected variable name: %v", nameV)
	}
	v, ok := Car(Cdr(Cdr(pair, true)))
	if !ok {
		return fmt.Errorf("syntax error: %v", pair)
	}
	err := scm.compileValue(v, false)
	if err != nil {
		return err
	}

	nameSym := scm.Intern(name.Name)
	b, ok := scm.env.Lookup(name.Name)
	if ok {
		instr := scm.addInstr(OpLocalSet, nil, b.Frame)
		instr.J = b.Index
	} else {
		instr := scm.addInstr(OpGlobalSet, nil, 0)
		instr.Sym = nameSym
	}

	return nil
}

func (scm *Scheme) compileIf(pair Pair, length int, tail bool) error {
	if length < 3 || length > 4 {
		return fmt.Errorf("if: syntax error")
	}
	cond, ok := Car(Cdr(pair, true))
	if !ok {
		return fmt.Errorf("if: syntax error")
	}
	t, ok := Car(Cdr(Cdr(pair, true)))
	if !ok {
		return fmt.Errorf("if: syntax error: consequent")
	}
	var f Value
	if length == 4 {
		f, ok = Car(Cdr(Cdr(Cdr(pair, true))))
		if !ok {
			return fmt.Errorf("if: syntax error: alternate")
		}
	}

	labelTrue := scm.newLabel()
	labelEnd := scm.newLabel()

	err := scm.compileValue(cond, false)
	if err != nil {
		return err
	}
	instr := scm.addInstr(OpIf, nil, 0)
	instr.J = labelTrue.I

	if length == 4 {
		err = scm.compileValue(f, tail)
		if err != nil {
			return err
		}
		if !tail {
			instr = scm.addInstr(OpJmp, nil, 0)
			instr.J = labelEnd.I
		}
	}

	scm.addLabel(labelTrue)
	err = scm.compileValue(t, tail)
	if err != nil {
		return err
	}
	if !tail {
		scm.addLabel(labelEnd)
	}

	return nil
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}

func isIdentifier(value Value, ok bool) (*Identifier, Value, bool) {
	if !ok {
		return nil, value, ok
	}
	id, ok := value.(*Identifier)
	return id, value, ok
}

// LambdaBody defines the lambda body and its location in the compiled
// bytecode.
type LambdaBody struct {
	Start int
	End   int
	Args  []*Identifier
	Body  Pair
	Env   *Env
}

// Env implements environment bindings.
type Env struct {
	Frames []EnvFrame
}

// NewEnv creates a new empty environment.
func NewEnv() *Env {
	return &Env{}
}

// Print prints the environment to standard output.
func (e *Env) Print() {
	fmt.Printf("Env:    depth=%v\n", len(e.Frames))
	for i := len(e.Frames) - 1; i >= 0; i-- {
		fmt.Printf("%7d", i)
		for k, v := range e.Frames[i] {
			fmt.Printf(" %v=%d.%d", k, v.Frame, v.Index)
		}
		fmt.Println()
	}
	fmt.Printf("-------\n")
}

// Empty tests if the environment is empty.
func (e *Env) Empty() bool {
	return e.Depth() == 0
}

// Depth returns the depth of the environment.
func (e *Env) Depth() int {
	return len(e.Frames)
}

// PushFrame pushes an environment frame.
func (e *Env) PushFrame() {
	e.Frames = append(e.Frames, make(EnvFrame))
}

// PopFrame pops the topmost environment frame.
func (e *Env) PopFrame() {
	e.Frames = e.Frames[:len(e.Frames)-1]
}

// ShiftDown shifts the environment frames down. The bottom-most
// elements goes to the top of the env.
func (e *Env) ShiftDown() {
	if len(e.Frames) < 2 {
		return
	}
	bottom := e.Frames[0]
	e.Frames = e.Frames[1:]
	e.Frames = append(e.Frames, bottom)
}

// Define defines the named symbol in the environment.
func (e *Env) Define(name string) (EnvBinding, error) {
	top := len(e.Frames) - 1
	_, ok := e.Frames[top][name]
	if ok {
		return EnvBinding{}, fmt.Errorf("symbol %s already defined", name)
	}
	b := EnvBinding{
		Frame: top,
		Index: len(e.Frames[top]),
	}
	e.Frames[top][name] = b
	return b, nil
}

// Lookup finds the symbol from the environment.
func (e *Env) Lookup(name string) (EnvBinding, bool) {
	for i := len(e.Frames) - 1; i >= 0; i-- {
		b, ok := e.Frames[i][name]
		if ok {
			return b, true
		}
	}
	return EnvBinding{}, false
}

// Push pushes the frames of the argument environment to the top of
// this environment.
func (e *Env) Push(o *Env) {
	for _, frame := range o.Frames {
		var names []string
		for k := range frame {
			names = append(names, k)
		}
		sort.Slice(names, func(i, j int) bool {
			return frame[names[i]].Index < frame[names[j]].Index
		})

		e.PushFrame()
		for _, name := range names {
			e.Define(name)
		}
	}
}

// EnvFrame implements an environment frame.
type EnvFrame map[string]EnvBinding

// EnvBinding defines symbol's location in the environment.
type EnvBinding struct {
	Frame int
	Index int
}

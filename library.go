//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"os"

	"github.com/markkurossi/scheme/pp"
	"github.com/markkurossi/scheme/types"
)

// Library implements a Scheme compilation unit.
type Library struct {
	scm       *Scheme
	Source    string
	Name      Value
	Body      *ASTSequence
	Exports   Value
	ExportAll bool
	Imports   Value
	Init      Code
	PCMap     PCMap

	lambdas   []*lambdaCompilation
	nextLabel int
	exported  map[string]*export
	current   *lambdaCompilation
}

func (lib *Library) String() string {
	return fmt.Sprintf("(library %v %v)", lib.Name, lib.Source)
}

// Scheme implements Value.Scheme.
func (lib *Library) Scheme() string {
	return fmt.Sprintf("#<library %s>", lib.Name.Scheme())
}

// Eq implements Value.Eq.
func (lib *Library) Eq(o Value) bool {
	olib, ok := o.(*Library)
	return ok && lib == olib
}

// Equal implements Value.Equal.
func (lib *Library) Equal(o Value) bool {
	return lib.Eq(o)
}

// Type implements Value.Type.
func (lib *Library) Type() *types.Type {
	return types.Any
}

// Unbox implements Value.Unbox.
func (lib *Library) Unbox() (Value, *types.Type) {
	return lib, lib.Type()
}

func (lib *Library) parseLibraryHeader(list []Pair) error {
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
	lib.Name = pair

	// Export.
	l, ok = ListPairs(list[2].Car())
	if !ok || len(l) == 0 || !IsSymbol(l[0].Car(), "export") {
		return list[2].Errorf("expected (export ...)")
	}
	for i := 1; i < len(l); i++ {
		if !Symbolp(l[i].Car()) {
			return l[i].Errorf("invalid export name: %v", l[i])
		}
	}
	lib.Exports = l[0].Cdr()

	// Import.
	pair, ok = list[3].Car().(Pair)
	if !ok {
		return list[3].Errorf("invalid library import: %v", list[3].Car())
	}
	_, ok = ListPairs(list[3].Car())
	if !ok {
		return list[3].Errorf("expected (import ...)")
	}
	lib.Imports, _ = Cdr(pair, true)

	return nil
}

// MapPC maps the program counter value to the source location.
func (lib *Library) MapPC(pc int) (source string, line int) {
	source = lib.Source

	if false {
		fmt.Printf("Library.MapPC: %v:%v\n", source, pc)
		for idx, pm := range lib.PCMap {
			fmt.Printf(" - %v\tPC=%v, Line=%v\n", idx, pm.PC, pm.Line)
		}
		lib.Init.Print(os.Stdout)
	}

	line = lib.PCMap.MapPC(pc)
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

// PrettyPrint formats the library Scheme code into the pp.Writer.
func (lib *Library) PrettyPrint(w pp.Writer) error {
	w.Header()
	last := 1
	for _, item := range lib.Body.Items {
		line := item.Locator().From().Line
		if line > last+1 {
			w.Println()
		}
		last = line

		item.PP(w)
		w.Println()
	}
	w.Trailer()

	return w.Error()
}

// Compile compiles the library into bytecode.
func (lib *Library) Compile(typeInfer bool) (Value, error) {
	var err error

	if typeInfer {
		inferer := NewInferer(lib.scm, lib.Body.Items)
		_, err = inferer.Infer(lib.Body)
		if err != nil {
			return nil, err
		}
	}

	err = lib.Body.Bytecode(lib)
	if err != nil {
		return nil, err
	}

	lib.addInstr(nil, OpReturn, nil, 0)

	// Compile lambdas.

	var pcmaps []PCMap

	for i := 0; i < len(lib.lambdas); i++ {
		lambda := lib.lambdas[i]
		pcmapStart := len(lib.PCMap)
		lib.current = lambda

		// Lambda body starts after the label.
		ofs := len(lib.Init)
		lambda.Start = ofs + 1
		lambda.Label = lib.addInstr(nil, OpLabel, nil, lambda.Start)

		for _, ast := range lambda.Body {
			err := ast.Bytecode(lib)
			if err != nil {
				return nil, err
			}
		}
		lib.addInstr(nil, OpReturn, nil, 0)
		lambda.End = len(lib.Init)

		pcmap := make(PCMap, len(lib.PCMap)-pcmapStart)
		copy(pcmap, lib.PCMap[pcmapStart:])
		for i := 0; i < len(pcmap); i++ {
			pcmap[i].PC -= lambda.Start
		}
		pcmaps = append(pcmaps, pcmap)
		lib.current = nil
	}

	// Collect label offsets
	labels := make(map[int]int)
	for idx, c := range lib.Init {
		if c.Op == OpLabel {
			labels[c.I] = idx
		}
	}

	// Patch code offsets.
	for i := 0; i < len(lib.Init); i++ {
		instr := lib.Init[i]
		switch instr.Op {
		case OpLambda:
			pcmap := pcmaps[instr.I]
			def := lib.lambdas[instr.I]

			var name string
			if def.Name != nil {
				name = def.Name.Name
			}
			t := def.Self.Type()
			if typeInferRuntime {
				if t.Enum != types.EnumLambda {
					return nil, fmt.Errorf("invalid lambda type: %v", t)
				}
			} else {
				t = &types.Type{
					Enum:   types.EnumLambda,
					Return: types.Unspecified,
				}
			}

			instr.I = def.Start
			instr.J = def.End
			instr.V = &LambdaImpl{
				Name:     name,
				Args:     def.Args,
				Return:   t.Return,
				Captures: def.Captures,
				Source:   lib.Source,
				Code:     lib.Init[def.Start:def.End],
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
	for k, v := range lib.exported {
		if v.id == nil {
			return nil, v.from.Errorf("exported symbol '%s' not defined", k)
		}
	}

	return &Lambda{
		Impl: &LambdaImpl{
			Return:   types.Any,
			Source:   lib.Source,
			Code:     lib.Init,
			PCMap:    lib.PCMap,
			Captures: true,
		},
	}, nil
}

func (lib *Library) addCall(from Locator, numArgs int, tail bool) {
	if numArgs >= 0 {
		lib.addInstr(from, OpConst, Int(numArgs), 0)
	}
	var i int
	if tail {
		i = 1
	}
	lib.addInstr(from, OpCall, nil, i)
}

func (lib *Library) addPushS(from Locator, size int, capture bool) {
	var op Operand
	if capture {
		op = OpPushE
	} else {
		op = OpPushS
	}
	lib.addInstr(from, op, nil, size)
}

func (lib *Library) addPopS(from Locator, size int, capture bool) {
	var op Operand
	if capture {
		op = OpPopE
	} else {
		op = OpPopS
	}
	instr := lib.addInstr(from, op, nil, size)
	if !capture {
		instr.J = 1
	}
}

func (lib *Library) addInstr(from Locator, op Operand, v Value, i int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
	}
	if from != nil {
		p := from.From()
		if len(lib.PCMap) == 0 ||
			lib.PCMap[len(lib.PCMap)-1].Line != p.Line {
			lib.PCMap = append(lib.PCMap, PCLine{
				PC:   len(lib.Init),
				Line: p.Line,
			})
		}
	}
	lib.Init = append(lib.Init, instr)
	return instr
}

func (lib *Library) addLabel(l *Instr) {
	lib.Init = append(lib.Init, l)
}

func (lib *Library) newLabel() *Instr {
	lib.nextLabel++
	return &Instr{
		Op: OpLabel,
		I:  lib.nextLabel - 1,
	}
}

func (lib *Library) setBinding(from Locator, b *EnvBinding) {
	if b.Frame.Type == TypeStack {
		lib.addInstr(from, OpLocalSet, nil, b.Frame.Index+b.Index)
	} else {
		instr := lib.addInstr(from, OpEnvSet, nil, b.Frame.Index)
		instr.J = b.Index
	}
}

type lambdaCompilation struct {
	Start       int
	Label       *Instr
	End         int
	Self        AST
	Name        *Symbol
	Args        Args
	ArgBindings []*EnvBinding
	Body        []AST
	Env         *Env
	MaxStack    int
	Captures    bool
}

//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"os"

	"github.com/markkurossi/scheme/types"
)

// Library implements a Scheme compilation unit.
type Library struct {
	c         *Parser
	Source    string
	Name      Value
	Body      *ASTSequence
	Exports   Value
	ExportAll bool
	Imports   Value
	Init      Code
	PCMap     PCMap
}

// Scheme implements Value.Scheme.
func (lib *Library) Scheme() string {
	return fmt.Sprintf("library %s", lib.Name.Scheme())
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
	if !ok || len(l) == 0 || !isNamedIdentifier(l[0].Car(), "export") {
		return list[2].Errorf("expected (export ...)")
	}
	for i := 1; i < len(l); i++ {
		_, ok := isIdentifier(l[i].Car())
		if !ok {
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

// Compile compiles the library into bytecode.
func (c *Parser) Compile(library *Library) (Value, error) {
	err := library.Body.Bytecode(c)
	if err != nil {
		return nil, err
	}

	c.addInstr(nil, OpReturn, nil, 0)

	// Compile lambdas.

	var pcmaps []PCMap

	for i := 0; i < len(c.lambdas); i++ {
		lambda := c.lambdas[i]
		pcmapStart := len(library.PCMap)

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

		pcmap := c.library.PCMap[pcmapStart:len(c.library.PCMap)]
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

	return &Lambda{
		Impl: &LambdaImpl{
			Return:   types.Any,
			Source:   library.Source,
			Code:     c.code,
			PCMap:    library.PCMap,
			Captures: true,
		},
	}, nil
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

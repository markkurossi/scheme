//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"math"
	"os"
	"strings"
)

// Scheme implements Scheme interpreter and virtual machine.
type Scheme struct {
	Stdout   io.Writer
	Parsing  bool
	compiled Code
	env      *Env
	lambdas  []*LambdaBody

	pc      int
	fp      int
	accu    Value
	stack   [][]Value
	symbols map[string]*Identifier
}

// New creates a new Scheme interpreter.
func New() (*Scheme, error) {
	scm := &Scheme{
		Stdout:  os.Stdout,
		symbols: make(map[string]*Identifier),
	}

	scm.DefineBuiltins(outputBuiltins)
	scm.DefineBuiltins(stringBuiltins)
	scm.DefineBuiltins(numberBuiltins)

	return scm, nil
}

// DefineBuiltins defines the built-in functions, defined in the
// argument array.
func (scm *Scheme) DefineBuiltins(builtins []Builtin) {
	for _, bi := range builtins {
		scm.DefineBuiltin(bi.Name, bi.Args, bi.Native)
	}
}

// DefineBuiltin defines a built-in native function.
func (scm *Scheme) DefineBuiltin(name string, args []string, native Native) {

	var minArgs, maxArgs int
	var usage []*Identifier
	var rest bool

	for _, arg := range args {
		usage = append(usage, &Identifier{
			Name: arg,
		})
		maxArgs++
		if arg[0] != '[' {
			minArgs++
		}
		if strings.HasSuffix(arg, "...") {
			rest = true
		}
	}
	if rest {
		maxArgs = math.MaxInt
	}

	sym := scm.Intern(name)
	sym.Global = &Lambda{
		Name:    name,
		Args:    usage,
		MinArgs: minArgs,
		MaxArgs: maxArgs,
		Native:  native,
	}
}

// EvalFile evaluates the scheme file.
func (scm *Scheme) EvalFile(file string) (Value, error) {
	in, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer in.Close()
	return scm.Eval(file, in)
}

// Eval evaluates the scheme source.
func (scm *Scheme) Eval(source string, in io.Reader) (Value, error) {
	code, err := scm.Compile(source, in)
	if err != nil {
		return nil, err
	}
	if false {
		fmt.Printf("Code:\n")
		for _, c := range code {
			fmt.Printf("%s\n", c)
		}
	}
	return scm.Execute(code)
}

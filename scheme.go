//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
)

// Scheme implements Scheme interpreter and virtual machine.
type Scheme struct {
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
		symbols: make(map[string]*Identifier),
	}

	scm.DefineBuiltins(outputBuiltins)
	scm.DefineBuiltins(stringBuiltins)

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

	for _, arg := range args {
		usage = append(usage, &Identifier{
			Name: arg,
		})
		maxArgs++
		if arg[0] != '[' {
			minArgs++
		}
	}

	sym := scm.Intern(name)
	sym.Global = &Lambda{
		Args:    usage,
		MinArgs: minArgs,
		MaxArgs: maxArgs,
		Native:  native,
	}
}

// EvalFile evaluates the scheme file.
func (scm *Scheme) EvalFile(file string) (Value, error) {
	code, err := scm.CompileFile(file)
	if err != nil {
		return nil, err
	}
	for _, c := range code {
		fmt.Printf("%s\n", c)
	}
	return scm.Execute(code)
}

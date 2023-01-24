//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"bytes"
	"embed"
	"fmt"
	"io"
	"math"
	"os"
	"path"
	"strings"
)

// runtime holds runtime functions defined in Scheme.
//
//go:embed runtime/*.scm
var runtime embed.FS

// Scheme implements Scheme interpreter and virtual machine.
type Scheme struct {
	Params  Params
	Stdout  *Port
	Stderr  *Port
	Parsing bool
	verbose bool

	pc      int
	fp      int
	accu    Value
	stack   [][]Value
	symbols map[string]*Identifier
}

// Params define the configuration parameters for Scheme.
type Params struct {
	// Verbose output.
	Verbose bool

	// NoRuntime specifies if the Scheme-implemented runtime is
	// initialized.
	NoRuntime bool
}

// New creates a new Scheme interpreter.
func New() (*Scheme, error) {
	return NewWithParams(Params{})
}

// NewWithParams creates a new Scheme interpreter with the parameters.
func NewWithParams(params Params) (*Scheme, error) {
	scm := &Scheme{
		Params:  params,
		Stdout:  NewPort(os.Stdout),
		Stderr:  NewPort(os.Stderr),
		symbols: make(map[string]*Identifier),
	}

	scm.DefineBuiltins(booleanBuiltins)
	scm.DefineBuiltins(characterBuiltins)
	scm.DefineBuiltins(debugBuiltins)
	scm.DefineBuiltins(listBuiltins)
	scm.DefineBuiltins(numberBuiltins)
	scm.DefineBuiltins(osBuiltins)
	scm.DefineBuiltins(outputBuiltins)
	scm.DefineBuiltins(procedureBuiltins)
	scm.DefineBuiltins(stringBuiltins)
	scm.DefineBuiltins(symbolBuiltins)
	scm.DefineBuiltins(vectorBuiltins)
	scm.DefineBuiltins(bytevectorBuiltins)
	scm.DefineBuiltins(loadBuiltins)

	scm.DefineBuiltins(rnrsUnicodeBuiltins)
	scm.DefineBuiltins(rnrsFilesBuiltins)

	if !scm.Params.NoRuntime {
		err := scm.loadRuntime("runtime")
		if err != nil {
			return nil, err
		}
	}

	return scm, nil
}

func (scm *Scheme) verbosef(format string, a ...interface{}) {
	if scm.verbose {
		fmt.Printf(format, a...)
	}
}

func (scm *Scheme) loadRuntime(dir string) error {
	entries, err := runtime.ReadDir(dir)
	if err != nil {
		return err
	}
	scm.verbosef("runtime:\n")
	for idx, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".scm") {
			continue
		}
		file := path.Join(dir, name)
		scm.verbosef("%6d : %v\n", idx, file)
		data, err := runtime.ReadFile(file)
		if err != nil {
			return err
		}
		_, err = scm.evalRuntime(file, bytes.NewReader(data))
		if err != nil {
			return err
		}
	}
	return nil
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
		if arg[0] != '[' && !strings.HasSuffix(arg, "...") {
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
		Name: name,
		Args: Args{
			Min:   minArgs,
			Max:   maxArgs,
			Fixed: usage,
		},
		Native: native,
	}
	sym.Flags |= FlagDefined
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
	library, err := scm.Load(source, in)
	if err != nil {
		return nil, err
	}
	sym := scm.Intern("scheme::init-library")

	return scm.Apply(sym.Global, []Value{library})
}

func (scm *Scheme) evalRuntime(source string, in io.Reader) (Value, error) {
	library, err := scm.Load(source, in)
	if err != nil {
		return nil, err
	}
	values, ok := ListValues(library)
	if !ok || len(values) != 5 {
		return nil, fmt.Errorf("invalid library: %v", library)
	}

	return scm.Apply(values[4], nil)
}

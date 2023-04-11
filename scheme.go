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

	hasRuntime bool

	pc      int
	accu    Value
	sp      int
	fp      int
	stack   []Value
	symbols map[string]*Identifier
}

// Params define the configuration parameters for Scheme.
type Params struct {
	// Verbose output.
	Verbose bool

	// NoRuntime specifies if the Scheme-implemented runtime is
	// initialized.
	NoRuntime bool

	// Do not warn when redefining global symbols.
	NoWarnDefine bool
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
		stack:   make([]Value, 4096), // XXX initial stack depth
		symbols: make(map[string]*Identifier),
	}

	scm.DefineBuiltins(booleanBuiltins)
	scm.DefineBuiltins(characterBuiltins)
	scm.DefineBuiltins(debugBuiltins)
	scm.DefineBuiltins(listBuiltins)
	scm.DefineBuiltins(numberBuiltins)
	scm.DefineBuiltins(procedureBuiltins)
	scm.DefineBuiltins(stringBuiltins)
	scm.DefineBuiltins(symbolBuiltins)
	scm.DefineBuiltins(vectorBuiltins)
	scm.DefineBuiltins(loadBuiltins)
	scm.DefineBuiltins(vmBuiltins)

	scm.DefineBuiltins(rnrsUnicodeBuiltins)
	scm.DefineBuiltins(rnrsBytevectorBuiltins)
	scm.DefineBuiltins(rnrsIOSimpleBuiltins)
	scm.DefineBuiltins(rnrsFilesBuiltins)
	scm.DefineBuiltins(rnrsMutablePairsBuiltins)
	scm.DefineBuiltins(rnrsMutableStringsBuiltins)
	scm.DefineBuiltins(rnrsProgramsBuiltins)

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
		_, err = scm.eval(file, bytes.NewReader(data))
		if err != nil {
			return err
		}
	}
	scm.hasRuntime = true

	return nil
}

// DefineBuiltins defines the built-in functions, defined in the
// argument array.
func (scm *Scheme) DefineBuiltins(builtins []Builtin) {
	for _, bi := range builtins {
		scm.DefineBuiltin(bi)
	}
}

// DefineBuiltin defines a built-in native function.
func (scm *Scheme) DefineBuiltin(builtin Builtin) {

	var minArgs, maxArgs int
	var usage []*Identifier
	var rest bool

	for _, arg := range builtin.Args {
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

	args := Args{
		Min:   minArgs,
		Max:   maxArgs,
		Fixed: usage,
	}

	sym := scm.Intern(builtin.Name)
	sym.Global = &Lambda{
		Name:   builtin.Name,
		Args:   args,
		Native: builtin.Native,
	}
	sym.Flags |= FlagDefined

	for _, alias := range builtin.Aliases {
		sym = scm.Intern(alias)
		sym.Global = &Lambda{
			Name:   alias,
			Args:   args,
			Native: builtin.Native,
		}
		sym.Flags |= FlagDefined
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
	if scm.hasRuntime {
		return scm.evalRuntime(source, in)
	}
	return scm.eval(source, in)
}

func (scm *Scheme) evalRuntime(source string, in io.Reader) (Value, error) {
	library, err := scm.Load(source, in)
	if err != nil {
		return nil, err
	}
	sym := scm.Intern("scheme::init-library")

	return scm.Apply(sym.Global, []Value{library})
}

func (scm *Scheme) eval(source string, in io.Reader) (Value, error) {
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

// Global returns the global value of the symbol.
func (scm *Scheme) Global(name string) (Value, error) {
	id, ok := scm.symbols[name]
	if !ok || id.Flags&FlagDefined == 0 {
		return nil, fmt.Errorf("undefined symbol '%s'", name)
	}
	return id.Global, nil
}

// SetGlobal sets the value of the global symbol. The function returns
// an error if the symbols was defined to be a FlagFinal. The symbol
// will became defined if it was undefined before the call.
func (scm *Scheme) SetGlobal(name string, value Value) error {
	id := scm.Intern(name)
	if id.Flags&FlagFinal != 0 {
		return fmt.Errorf("can't reset final symbol '%s'", name)
	}
	id.Flags |= FlagDefined
	id.Global = value
	return nil
}

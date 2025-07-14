//
// Copyright (c) 2022-2025 Markku Rossi
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

	"github.com/markkurossi/scheme/pp"
	"github.com/markkurossi/scheme/types"
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

	hasRuntime bool

	nextTypeVar int

	pc      int
	sp      int
	fp      int
	stack   []Value
	symbols map[string]*Symbol
	frameFL *Frame
}

// Params define the configuration parameters for Scheme.
type Params struct {
	// Verbosity level.
	Verbosity int

	// Quiet output.
	Quiet bool

	// NoRuntime specifies if the Scheme-implemented runtime is
	// initialized.
	NoRuntime bool

	// Do not warn when redefining global symbols.
	NoWarnDefine bool

	// Pragmas.
	Pragma  Pragmas
	pragmas []Pragmas
}

// Verbose returns true if verbose output is enabled.
func (p *Params) Verbose() bool {
	return p.Verbosity > 0
}

// Verbose2 returns true if verbosity is 2 or more.
func (p *Params) Verbose2() bool {
	return p.Verbosity >= 2
}

// PushScope push a new compilation scope and saves the current
// parameters.
func (p *Params) PushScope() {
	p.pragmas = append(p.pragmas, p.Pragma)
}

// PopScope pops a compilation scope and restores saved parameters.
func (p *Params) PopScope() {
	if len(p.pragmas) == 0 {
		panic("scope stack underflow")
	}
	p.Pragma = p.pragmas[len(p.pragmas)-1]
	p.pragmas = p.pragmas[:len(p.pragmas)-1]
}

// Pragmas define compilation pragmas.
type Pragmas struct {
	// Do not check boolean expressions in boolean contexts.
	NoCheckBooleanExprs bool

	// Enable verbose typechecks.
	VerboseTypecheck bool
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
		symbols: make(map[string]*Symbol),
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
	scm.DefineBuiltins(rnrsEvalBuiltins)

	scm.DefineBuiltins(srfi28Builtins)

	if !scm.Params.NoRuntime {
		err := scm.loadRuntime("runtime")
		if err != nil {
			return nil, err
		}
	}

	return scm, nil
}

func (scm *Scheme) verbosef(format string, a ...interface{}) {
	if scm.Params.Verbose() {
		scm.Stdout.Printf(format, a...)
	}
}

const typeInferRuntime = true

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
		_, err = scm.eval(file, bytes.NewReader(data), typeInferRuntime)
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

	if builtin.Return == nil {
		panic(fmt.Sprintf("builtin %v: no return type defined", builtin.Name))
	}

	var minArgs, maxArgs int
	var usage []*TypedName
	var rest bool

	for _, arg := range builtin.Args {
		typ, name, err := types.Parse(arg)
		if err != nil {
			fmt.Printf("- %v %v: %v\n", builtin.Name, builtin.Args, err)
		}
		usage = append(usage, &TypedName{
			Name: name,
			Type: typ,
		})
		maxArgs++
		if typ.Kind == types.Fixed {
			minArgs++
		}
		if typ.Kind == types.Rest {
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

	lambda := &Lambda{
		Impl: &LambdaImpl{
			Name:         builtin.Name,
			Args:         args,
			Return:       builtin.Return,
			Native:       builtin.Native,
			Parametrizer: builtin.Parametrize,
		},
	}
	sym := scm.Intern(builtin.Name)
	sym.GlobalType = lambda.Type()
	sym.Global = lambda
	sym.Flags |= FlagDefined
	sym.Flags |= builtin.Flags

	for _, alias := range builtin.Aliases {
		as := scm.Intern(alias)
		as.GlobalType = sym.GlobalType
		as.Global = &Lambda{
			Impl: &LambdaImpl{
				Name:   alias,
				Args:   args,
				Return: builtin.Return,
				Native: builtin.Native,
			},
		}
		as.Flags |= FlagDefined
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
	return scm.eval(source, in, true)
}

func (scm *Scheme) evalRuntime(source string, in io.Reader) (Value, error) {
	library, err := scm.Load(source, in)
	if err != nil {
		return nil, err
	}
	sym := scm.Intern("scheme::init-library")

	return scm.Apply(sym.Global, []Value{library, Boolean(true)})
}

func (scm *Scheme) eval(source string, in io.Reader, typeInfer bool) (
	Value, error) {

	library, err := scm.Load(source, in)
	if err != nil {
		return nil, err
	}
	values, ok := ListValues(library)
	if !ok || len(values) != 5 {
		return nil, fmt.Errorf("invalid library: %v", library)
	}
	lib, ok := values[4].(*Library)
	if !ok {
		return nil, fmt.Errorf("invalid library: %T", values[4])
	}

	init, err := lib.Compile(typeInfer)
	if err != nil {
		return nil, err
	}

	return scm.Apply(init, nil)
}

// Vet checks the source for errors.
func (scm *Scheme) Vet(source string, in io.Reader) error {
	library, err := scm.Load(source, in)
	if err != nil {
		return err
	}
	sym := scm.Intern("scheme::init-library")

	_, err = scm.Apply(sym.Global, []Value{library, Boolean(false)})
	if err != nil {
		return err
	}
	return err
}

// PrettyPrint formats the souce code into the pp.Writer.
func (scm *Scheme) PrettyPrint(source string, in io.Reader,
	w pp.Writer) error {

	library, err := scm.Load(source, in)
	if err != nil {
		return err
	}
	sym := scm.Intern("scheme::init-library")

	_, err = scm.Apply(sym.Global, []Value{library, Boolean(false)})
	if err != nil {
		return err
	}
	values, ok := ListValues(library)
	if !ok || len(values) != 5 {
		return fmt.Errorf("invalid library: %v", library)
	}
	lib, ok := values[4].(*Library)
	if !ok {
		return fmt.Errorf("invalid library: %T", values[4])
	}
	return lib.PrettyPrint(w)
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
	if id.Flags&FlagConst != 0 {
		return fmt.Errorf("can't reset final symbol '%s'", name)
	}
	id.Flags |= FlagDefined
	id.Global = value
	return nil
}

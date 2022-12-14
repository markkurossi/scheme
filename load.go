//
// Copyright (c) 2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"os"
	"path"
)

var loadBuiltins = []Builtin{
	{
		Name: "scheme::load",
		Args: []string{"caller", "filename"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			caller, ok := args[0].(String)
			if !ok {
				return nil, l.Errorf("invalid caller: %v", args[0])
			}
			f, ok := args[1].(String)
			if !ok {
				return nil, l.Errorf("invalid filename: %v", args[1])
			}
			file := string(f)
			if !path.IsAbs(file) {
				file = path.Join(path.Dir(string(caller)), file)
			}
			if scm.Params.Verbose {
				fmt.Printf("load: %v\n", file)
			}
			return scm.LoadFile(file)
		},
	},
	{
		Name: "scheme::stack-trace",
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			stack := scm.StackTrace()

			var result, tail Pair

			for _, frame := range stack {
				p := NewPair(
					NewPair(String(frame.Source),
						NewNumber(0, frame.Line)),
					nil)
				if tail == nil {
					result = p
				} else {
					tail.SetCdr(p)
				}
				tail = p
			}

			return result, nil
		},
	},
}

// LoadFile loads and compiles the file.
func (scm *Scheme) LoadFile(file string) (Value, error) {
	in, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer in.Close()
	return scm.Load(file, in)
}

// Load loads and compiles the input.
func (scm *Scheme) Load(source string, in io.Reader) (Value, error) {
	c := NewCompiler(scm)

	module, err := c.Compile(source, in)
	if err != nil {
		return nil, err
	}
	if false {
		fmt.Printf("Code:\n")
		for _, c := range module.Init {
			fmt.Printf("%s\n", c)
		}
	}
	var exports Pair
	var tail Pair
	for _, export := range module.Exports {
		p := NewPair(String(export), nil)
		if tail == nil {
			exports = p
		} else {
			tail.SetCdr(p)
		}
		tail = p
	}

	return NewPair(&Identifier{Name: "library"},
		NewPair(exports,
			NewPair(nil, // XXX imports
				NewPair(
					&Lambda{
						Source: module.Source,
						Code:   module.Init,
						PCMap:  module.PCMap,
					},
					nil)))), nil
}

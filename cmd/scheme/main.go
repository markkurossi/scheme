//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"strings"

	"github.com/markkurossi/scheme"
	"github.com/peterh/liner"
)

func main() {
	log.SetFlags(0)

	replp := flag.Bool("repl", false, "read-eval-print-loop")
	verbose := flag.Bool("v", false, "verbose output")
	noRuntime := flag.Bool("no-runtime", false, "do not load Scheme runtime")
	bc := flag.Bool("bc", false, "compile scheme into bytecode")
	flag.Parse()

	fmt.Printf("Go Scheme Version 0.0\n")

	scm, err := scheme.NewWithParams(scheme.Params{
		Verbose:   *verbose,
		NoRuntime: *noRuntime,
	})
	if err != nil {
		fmt.Printf("scheme.New: %v\n", err)
		return
	}

	for _, arg := range flag.Args() {
		if *bc {
			err = bytecode(scm, arg)
		} else {
			_, err = scm.EvalFile(arg)
		}
		if err != nil {
			log.Fatalf("%s\n", err)
		}
	}
	if *replp || len(flag.Args()) == 0 {
		repl(scm)
	}
}

func bytecode(scm *scheme.Scheme, file string) error {
	in, err := os.Open(file)
	if err != nil {
		return err
	}
	defer in.Close()

	c := scheme.NewCompiler(scm)

	module, err := c.Compile(file, in)
	if err != nil {
		return err
	}
	module.Init.Print()
	return nil
}

func repl(scm *scheme.Scheme) {
	input := &input{
		scm:   scm,
		liner: liner.NewLiner(),
	}
	defer input.liner.Close()

	for {
		input.count = 0
		v, err := scm.Eval("input", input)
		if err != nil {
			fmt.Printf("%v\n", err)
			continue
		}
		if scm.Parsing {
			fmt.Println()
			break
		}
		fmt.Printf(" => %v\n", v)
		input.SaveHistory()
	}
}

type input struct {
	scm   *scheme.Scheme
	count int
	liner *liner.State
	buf   []byte
	line  []byte
}

func (i *input) Read(p []byte) (n int, err error) {
	if len(i.buf) == 0 {
		if !i.scm.Parsing {
			return 0, io.EOF
		}

		prompt := "scm > "
		if i.count > 0 {
			prompt = "    >   "
		}

		var line string
		line, err = i.liner.Prompt(prompt)
		if err != nil {
			return
		}
		i.buf = []byte(line)
		i.buf = append(i.buf, ' ')
		i.count++
	}
	n = copy(p, i.buf)
	i.line = append(i.line, i.buf[:n]...)

	i.buf = i.buf[n:]

	return
}

func (i *input) SaveHistory() {
	if len(i.line) > 0 {
		i.liner.AppendHistory(strings.TrimSpace(string(i.line)))
	}
	i.line = nil
}

//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package main

import (
	"fmt"
	"io"

	"github.com/markkurossi/scheme"
	"github.com/peterh/liner"
)

func main() {
	fmt.Printf("Hello, Scheme!\n")

	scm, err := scheme.New()
	if err != nil {
		fmt.Printf("scheme.New: %v\n", err)
		return
	}

	input := &input{
		scm:   scm,
		liner: liner.NewLiner(),
	}
	defer input.liner.Close()

	for {
		v, err := scm.Eval("input", input)
		if err != nil {
			fmt.Printf("scheme.Eval: %v\n", err)
			return
		}
		if scm.Parsing {
			fmt.Println()
			break
		}
		fmt.Printf(" => %v\n", v)
	}
}

type input struct {
	scm   *scheme.Scheme
	liner *liner.State
	buf   []byte
}

func (i *input) Read(p []byte) (n int, err error) {
	if len(i.buf) == 0 {
		if !i.scm.Parsing {
			return 0, io.EOF
		}

		var line string
		line, err = i.liner.Prompt("scm > ")
		if err != nil {
			return
		}
		i.buf = []byte(line)
		i.buf = append(i.buf, '\n')
	}
	n = copy(p, i.buf)
	i.buf = i.buf[n:]
	return
}

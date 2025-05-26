//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package main

import (
	"flag"
	"fmt"
	"io"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"runtime/pprof"
	"strings"

	"github.com/markkurossi/scheme"
	"github.com/markkurossi/scheme/pp"
	"github.com/peterh/liner"
)

type keyvalue struct {
	key string
	val bool
}

type pragma []keyvalue

func (p *pragma) String() string {
	var result string

	for i, pragma := range *p {
		if i > 0 {
			result += " "
		}
		result += fmt.Sprintf("%v=%v", pragma.key, pragma.val)
	}
	return result
}

func (p *pragma) Set(value string) error {
	parts := strings.Split(value, "=")

	var name string
	var val bool

	switch len(parts) {
	case 1:
		name = parts[0]
		val = true

	case 2:
		name = parts[0]
		switch parts[1] {
		case "#t", "1", "true":
			val = true
		case "#f", "0", "false":
			val = false
		default:
			return fmt.Errorf("pragma %s: invalid boolean value '%s'",
				name, parts[1])
		}

	default:
		return fmt.Errorf("invalid pragma '%s'", value)
	}
	*p = append(*p, keyvalue{
		key: name,
		val: val,
	})
	return nil
}

var pragmas pragma

type verbosity int

func (v *verbosity) String() string {
	if v == nil {
		return "0"
	}
	return fmt.Sprintf("%v", *v)
}

func (v *verbosity) Set(value string) error {
	*v++
	return nil
}

func (v *verbosity) IsBoolFlag() bool {
	return true
}

var verbose verbosity

func init() {
	flag.Var(&pragmas, "pragma", "define pragma where value is: key=value")
	flag.Var(&verbose, "v", "verbose output")
}

func main() {
	log.SetFlags(0)

	replp := flag.Bool("repl", false, "read-eval-print-loop")
	noRuntime := flag.Bool("no-runtime", false, "do not load Scheme runtime")
	bc := flag.Bool("bc", false, "compile scheme into bytecode")
	cpuprofile := flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile := flag.String("memprofile", "",
		"write memory profile to `file`")
	pp := flag.String("pp", "", "pretty-print scheme")
	flag.Parse()

	if len(*cpuprofile) > 0 {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal("could not create CPU profile: ", err)
		}
		defer f.Close()
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal("could not start CPU profile: ", err)
		}
		defer pprof.StopCPUProfile()
	}

	params := scheme.Params{
		Verbosity: int(verbose),
		NoRuntime: *noRuntime,
	}
	for _, p := range pragmas {
		switch p.key {
		case "verbose-typecheck":
			params.Pragma.VerboseTypecheck = p.val

		default:
			log.Fatalf("unknown pragma: %v", p.key)
		}
	}

	scm, err := scheme.NewWithParams(params)
	if err != nil {
		fmt.Printf("scheme.New: %v\n", err)
		return
	}

	for idx, arg := range flag.Args() {
		switch arg {
		case "vet":
			err = vet(scm, flag.Args()[idx+1:])
			if err != nil {
				log.Fatal(err)
			}
			return

		case "test":
			err = test(scm, flag.Args()[idx+1:])
			if err != nil {
				log.Fatal(err)
			}
			return

		default:
			// Process argument as scheme file.
			if *bc {
				err = bytecode(scm, arg)
			} else if len(*pp) > 0 {
				err = prettyPrint(scm, *pp, arg)
			} else {
				_, err = scm.EvalFile(arg)
			}
			if err != nil {
				log.Fatalf("%s\n", err)
			}
		}
	}
	if *replp || len(flag.Args()) == 0 {
		fmt.Printf("Go Scheme Version 0.0\n")
		repl(scm)
	} else if len(*memprofile) > 0 {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal("could not create memory profile: ", err)
		}
		defer f.Close()
		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Fatal("could not write memory profile: ", err)
		}
	}
}

func vet(scm *scheme.Scheme, files []string) error {
	if len(files) == 0 {
		const root = "."
		err := filepath.WalkDir(root,
			func(path string, d fs.DirEntry, err error) error {
				if err != nil {
					return err
				}
				if path != root && d.IsDir() && false {
					return fs.SkipDir
				}
				if strings.HasSuffix(path, ".scm") {
					files = append(files, path)
				}
				return nil
			})
		if err != nil {
			return err
		}
	}
	for _, file := range files {
		err := vetFile(scm, file)
		if err != nil {
			return err
		}
	}
	return nil
}

func vetFile(scm *scheme.Scheme, file string) error {
	in, err := os.Open(file)
	if err != nil {
		return err
	}
	defer in.Close()

	return scm.Vet(file, in)
}

func test(scm *scheme.Scheme, files []string) error {
	lambda, err := scm.Eval("{input}", strings.NewReader(`(import (scheme test))
scheme::test::runner
`))
	if err != nil {
		return err
	}

	var args []scheme.Value
	for _, f := range files {
		args = append(args, scheme.String(f))
	}

	_, err = scm.Apply(lambda, args)
	return err
}

func bytecode(scm *scheme.Scheme, file string) error {
	in, err := os.Open(file)
	if err != nil {
		return err
	}
	defer in.Close()

	var outName string
	if strings.HasSuffix(file, ".scm") {
		outName = file[:len(file)-4] + ".sbc"
	} else {
		outName = file + ".sbc"
	}
	out, err := os.Create(outName)
	if err != nil {
		return err
	}
	defer out.Close()

	c := scheme.NewParser(scm)

	library, err := c.Parse(file, in)
	if err != nil {
		return err
	}
	v, err := library.Compile(true)
	if err != nil {
		return err
	}
	lambda, ok := v.(*scheme.Lambda)
	if !ok {
		return fmt.Errorf("unexpected init: %v", v)
	}

	lambda.Impl.Code.Print(out)
	return nil
}

func prettyPrint(scm *scheme.Scheme, format string, file string) error {
	in, err := os.Open(file)
	if err != nil {
		return err
	}
	defer in.Close()

	var outName string
	if strings.HasSuffix(file, ".scm") {
		outName = file[:len(file)-4]
	} else {
		outName = file
	}
	outName += "." + format
	out, err := os.Create(outName)
	if err != nil {
		return err
	}
	defer out.Close()

	var w pp.Writer
	switch format {
	case "html":
		w = pp.NewHTML(out, file)
	default:
		return fmt.Errorf("unknown pretty-print format '%s'", format)
	}

	return scm.PrettyPrint(file, in, w)
}

func repl(scm *scheme.Scheme) {
	input := &input{
		scm:   scm,
		liner: liner.NewLiner(),
	}
	defer input.liner.Close()

	for {
		input.count = 0
		v, err := scm.Eval("repl", input)
		if err != nil {
			fmt.Printf("%v\n", err)
			input.SaveHistory()
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

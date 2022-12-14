//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
)

// Port implements Scheme ports.
type Port struct {
	Native interface{}
}

// NewPort creates a new port for the native I/O channel.
func NewPort(native interface{}) *Port {
	return &Port{
		Native: native,
	}
}

// Printf prints formatted output to the port.
func (p *Port) Printf(format string, a ...interface{}) (n int, err error) {
	writer, ok := p.Native.(io.Writer)
	if ok {
		return fmt.Fprintf(writer, format, a...)
	}
	return 0, fmt.Errorf("invalid output port: %s", p)
}

// Println prints newline-terminated values to the port.
func (p *Port) Println(a ...interface{}) (n int, err error) {
	writer, ok := p.Native.(io.Writer)
	if ok {
		return fmt.Fprintln(writer, a...)
	}
	return 0, fmt.Errorf("invalid output port: %s", p)
}

// Scheme returns the value as a Scheme string.
func (p *Port) Scheme() string {
	return p.String()
}

func (p *Port) String() string {
	_, isInput := p.Native.(io.Reader)
	_, isOutput := p.Native.(io.Writer)

	if isInput && isOutput {
		return fmt.Sprintf("#<i/o-port %p>", p)
	} else if isInput {
		return fmt.Sprintf("#<input-port %p>", p)
	} else {
		return fmt.Sprintf("#<output-port %p>", p)
	}
}

// Eq tests if the argument value is eq? to this value.
func (p *Port) Eq(o Value) bool {
	op, ok := o.(*Port)
	if !ok {
		return false
	}
	return p.Native == op.Native
}

// Equal tests if the argument value is equal to this number.
func (p *Port) Equal(o Value) bool {
	return p.Eq(o)
}

var outputBuiltins = []Builtin{
	{
		Name: "input-port?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return Boolean(false), nil
			}
			_, ok = port.Native.(io.Reader)
			return Boolean(ok), nil
		},
	},
	{
		Name: "output-port?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return Boolean(false), nil
			}
			_, ok = port.Native.(io.Writer)
			return Boolean(ok), nil
		},
	},
	{
		Name: "current-output-port",
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return scm.Stdout, nil
		},
	},
	{
		Name: "current-error-port",
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return scm.Stderr, nil
		},
	},
	{
		Name: "display",
		Args: []string{"obj", "[port]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var ok bool
			port := scm.Stdout

			if len(args) == 2 {
				port, ok = args[1].(*Port)
				if !ok {
					return nil, l.Errorf("invalid output port: %v", args[1])
				}
			}
			_, err := port.Printf("%v", args[0])
			if err != nil {
				return nil, l.Errorf("%v", err)
			}
			return nil, nil
		},
	},
	{
		Name: "newline",
		Args: []string{"[port]"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			var ok bool
			port := scm.Stdout

			if len(args) == 1 {
				port, ok = args[0].(*Port)
				if !ok {
					return nil, l.Errorf("invalid output port: %v", args[0])
				}
			}
			_, err := port.Println()
			if err != nil {
				return nil, err
			}
			return nil, nil
		},
	},
}

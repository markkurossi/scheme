//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"io"
	"os"

	"github.com/markkurossi/scheme/types"
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

func (p *Port) Read(buf []byte) (int, error) {
	reader, ok := p.Native.(io.Reader)
	if ok {
		return reader.Read(buf)
	}
	return 0, fmt.Errorf("invalid input port: %s", p)
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

// Type implements Value.Type.
func (p *Port) Type() *types.Type {
	return types.Port
}

// Unbox implements Value.Unbox.
func (p *Port) Unbox() (Value, *types.Type) {
	return p, p.Type()
}

// EOFObject implements end-of-file object.
type EOFObject struct {
	eof error
}

// EOF implements the end-of-file object.
var EOF = &EOFObject{
	eof: io.EOF,
}

// Scheme implements Value.Scheme.
func (p *EOFObject) Scheme() string {
	return p.String()
}

func (p *EOFObject) String() string {
	return "#<end-of-file>"
}

// Eq tests if the argument value is eq? to this value.
func (p *EOFObject) Eq(o Value) bool {
	_, ok := o.(*EOFObject)
	return ok
}

// Equal tests if the argument value is equal to this number.
func (p *EOFObject) Equal(o Value) bool {
	return p.Eq(o)
}

// Type implements Value.Type.
func (p *EOFObject) Type() *types.Type {
	return types.EOF
}

// Unbox implements Value.Unbox.
func (p *EOFObject) Unbox() (Value, *types.Type) {
	return p, p.Type()
}

var rnrsIOSimpleBuiltins = []Builtin{
	{
		Name:   "eof-object",
		Return: types.EOF,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return EOF, nil
		},
	},
	{
		Name:   "eof-object?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(*EOFObject)
			return Boolean(ok), nil
		},
	},
	{
		Name:   "input-port?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return Boolean(false), nil
			}
			_, ok = port.Native.(io.Reader)
			return Boolean(ok), nil
		},
	},
	{
		Name:   "output-port?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return Boolean(false), nil
			}
			_, ok = port.Native.(io.Writer)
			return Boolean(ok), nil
		},
	},
	{
		Name:   "current-input-port",
		Return: types.Port,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return scm.Stdin, nil
		},
	},
	{
		Name:   "current-output-port",
		Return: types.Port,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return scm.Stdout, nil
		},
	},
	{
		Name:   "current-error-port",
		Return: types.Port,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return scm.Stderr, nil
		},
	},
	{
		Name:        "open-input-file",
		Permissions: PermFSRead,
		Args:        []string{"filename<string>"},
		Return:      types.Port,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			filename, ok := args[0].(String)
			if !ok {
				return nil, fmt.Errorf("invalid filename: %v", args[0])
			}
			f, err := os.Open(string(filename))
			if err != nil {
				return nil, err
			}
			return NewPort(f), nil
		},
	},
	{
		Name:   "close-input-port",
		Args:   []string{"port"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			port, ok := args[0].(*Port)
			if !ok {
				return nil, fmt.Errorf("invalid port: %v", args[1])
			}
			closer, ok := port.Native.(io.Closer)
			if !ok {
				return nil, nil
			}
			return nil, closer.Close()
		},
	},
	{
		Name:   "newline",
		Args:   []string{"[port]"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var ok bool
			port := scm.Stdout

			if len(args) == 1 {
				port, ok = args[0].(*Port)
				if !ok {
					return nil, fmt.Errorf("invalid output port: %v", args[0])
				}
			}
			_, err := port.Println()
			if err != nil {
				return nil, err
			}
			return nil, nil
		},
	},
	{
		Name:   "display",
		Args:   []string{"obj", "[port]"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var ok bool
			port := scm.Stdout

			if len(args) == 2 {
				port, ok = args[1].(*Port)
				if !ok {
					return nil, fmt.Errorf("invalid output port: %v", args[1])
				}
			}
			_, err := port.Printf("%v", ToString(args[0]))
			if err != nil {
				return nil, fmt.Errorf("%v", err)
			}
			return nil, nil
		},
	},
	{
		Name:   "write",
		Args:   []string{"obj", "[port]"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var ok bool
			port := scm.Stdout

			if len(args) == 2 {
				port, ok = args[1].(*Port)
				if !ok {
					return nil, fmt.Errorf("invalid output port: %v", args[1])
				}
			}
			_, err := port.Printf("%v", ToScheme(args[0]))
			if err != nil {
				return nil, fmt.Errorf("%v", err)
			}
			return nil, nil
		},
	},
}

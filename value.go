//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"math"
	"strings"
)

var (
	_ Value = &Vector{}
	_ Value = &Identifier{}
	_ Value = Keyword(0)
	_ Value = &Number{}
	_ Value = Boolean(true)
	_ Value = &String{}
	_ Value = Character('@')
	_ Value = &Lambda{}
	_ Value = &Frame{}
	_ Value = &Port{}
)

// Value implements a Scheme value.
type Value interface {
	Scheme() string
	Eq(o Value) bool
	Equal(o Value) bool
}

// Flags define symbol flags.
type Flags int

// Symbol flags.
const (
	FlagDefined Flags = 1 << iota
	FlagFinal
)

// Identifier implements identifier values.
type Identifier struct {
	Name   string
	Point  Point
	Global Value
	Flags  Flags
}

// Scheme returns the value as a Scheme string.
func (v *Identifier) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v *Identifier) Eq(o Value) bool {
	return v.Equal(o)
}

// Equal tests if the argument value is equal to this value.
func (v *Identifier) Equal(o Value) bool {
	ov, ok := o.(*Identifier)
	return ok && v.Name == ov.Name
}

func (v *Identifier) String() string {
	return v.Name
}

// Lambda implements lambda values.
type Lambda struct {
	Name    string
	Args    Args
	Capture int
	Locals  [][]Value
	Native  Native
	Source  string
	Code    Code
	PCMap   PCMap
	Body    []Pair
}

// Args specify lambda arguments.
type Args struct {
	Min   int
	Max   int
	Fixed []*Identifier
	Rest  *Identifier
}

func (args Args) String() string {
	if len(args.Fixed) == 0 {
		if args.Rest == nil {
			return "()"
		}
		return args.Rest.Name
	}

	var str strings.Builder

	str.WriteRune('(')

	for idx, arg := range args.Fixed {
		if idx > 0 {
			str.WriteRune(' ')
		}
		str.WriteString(arg.Name)
	}
	if args.Rest != nil {
		str.WriteString(" . ")
		str.WriteString(args.Rest.Name)
	}
	str.WriteRune(')')
	return str.String()
}

// Equal tests if the arguments are equal.
func (args Args) Equal(o Args) bool {
	return args.Min == o.Min && args.Max == o.Max
}

// Init initializes argument limits and checks that all argument names
// are unique.
func (args *Args) Init() {
	args.Min = len(args.Fixed)
	if args.Rest != nil {
		args.Max = math.MaxInt
	} else {
		args.Max = args.Min
	}
}

// Seen implements uniqueness checker for argument names.
type Seen map[string]bool

// NewSeen creates a new argument name checker.
func NewSeen() Seen {
	return make(Seen)
}

// Add adds the argument name to the name checker.
func (seen Seen) Add(name string) error {
	_, ok := seen[name]
	if ok {
		return fmt.Errorf("argument '%s' already seen", name)
	}
	seen[name] = true
	return nil
}

// Scheme returns the value as a Scheme string.
func (v *Lambda) Scheme() string {
	return v.String()
}

// Eq tests if the argument value is eq? to this value.
func (v *Lambda) Eq(o Value) bool {
	return v == o
}

// Equal tests if the argument value is equal to this value.
func (v *Lambda) Equal(o Value) bool {
	ov, ok := o.(*Lambda)
	if !ok {
		return false
	}
	if !v.Args.Equal(ov.Args) {
		return false
	}
	if v.Capture != ov.Capture {
		return false
	}
	if v.Native == nil && ov.Native != nil {
		return false
	}
	if v.Native != nil && ov.Native == nil {
		return false
	}
	if len(v.Body) != len(ov.Body) {
		return false
	}
	for idx, vv := range v.Body {
		if !vv.Equal(ov.Body[idx]) {
			return false
		}
	}
	return true
}

func (v *Lambda) String() string {
	return v.Signature(true)
}

// Signature prints the lambda signature with optional lambda body.
func (v *Lambda) Signature(body bool) string {
	var str strings.Builder

	if v.Native != nil {
		str.WriteRune('(')
		str.WriteString(v.Name)
		str.WriteString(" ")
	} else {
		str.WriteString("(lambda ")
	}
	str.WriteString(v.Args.String())

	if v.Native != nil {
		str.WriteString(" {native}")
	} else if len(v.Body) > 0 {
		if body {
			for _, pair := range v.Body {
				str.WriteRune(' ')
				str.WriteString(fmt.Sprintf("%v", pair.Car()))
			}
		} else {
			str.WriteString(" ...")
		}
	} else if v.Code != nil {
		str.WriteString(" {compiled}")
	}
	str.WriteRune(')')

	return str.String()
}

// Errorf returns an error with information about the lambda function.
func (v *Lambda) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if len(v.Name) != 0 {
		return fmt.Errorf("%s: %s", v.Name, msg)
	}
	return errors.New(msg)
}

// MapPC maps the program counter value to the source location.
func (v *Lambda) MapPC(pc int) (source string, line int) {
	source = v.Source

	if false {
		fmt.Printf("MapPC: %v:%v\n", source, pc)
		for idx, pm := range v.PCMap {
			fmt.Printf(" - %v\tPC=%v, Line=%v\n", idx, pm.PC, pm.Line)
		}
		v.Code.Print()
	}

	line = v.PCMap.MapPC(pc)
	return
}

// Native implements native functions.
type Native func(scm *Scheme, l *Lambda, args []Value) (Value, error)

// Builtin defines a built-in native function.
type Builtin struct {
	Name    string
	Args    []string
	MinArgs int
	MaxArgs int
	Native  Native
}

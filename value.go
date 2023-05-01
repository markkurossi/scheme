//
// Copyright (c) 2022-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"math"
	"os"
	"strings"

	"github.com/markkurossi/scheme/types"
)

var (
	_ Value = &Vector{}
	_ Value = &Identifier{}
	_ Value = Keyword(0)
	_ Value = Boolean(true)
	_ Value = String("string")
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

// ToString returns a display representation of the value.
func ToString(v Value) string {
	if v == nil {
		return "'()"
	}
	return fmt.Sprintf("%v", v)
}

// ToScheme returns a Scheme representation of the value.
func ToScheme(v Value) string {
	if v == nil {
		return "'()"
	}
	return v.Scheme()
}

// Flags define symbol flags.
type Flags int

// Symbol flags.
const (
	FlagDefined Flags = 1 << iota
	FlagConst
)

func (f Flags) String() string {
	var result string
	if f&FlagDefined != 0 {
		result += " defined"
	}
	if f&FlagConst != 0 {
		result += " const"
	}
	return strings.TrimSpace(result)
}

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
	Name     string
	Args     Args
	Captures bool
	Capture  *VMEnvFrame
	Native   Native
	Source   string
	Code     Code
	MaxStack int
	PCMap    PCMap
	Body     []Pair
}

// Args specify lambda arguments.
type Args struct {
	Min   int
	Max   int
	Fixed []*TypedName
	Rest  *TypedName
}

// TypedName defines name with type information.
type TypedName struct {
	Name string
	Type *types.Type
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
	if v.Captures != ov.Captures {
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
	return v.Signature(false)
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
		v.Code.Print(os.Stdout)
	}

	line = v.PCMap.MapPC(pc)
	return
}

// Native implements native functions.
type Native func(scm *Scheme, l *Lambda, args []Value) (Value, error)

// Builtin defines a built-in native function.
type Builtin struct {
	Name    string
	Aliases []string
	Args    []string
	Flags   Flags
	MinArgs int
	MaxArgs int
	Native  Native
}

//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"math"
	"os"
	"strings"

	"github.com/markkurossi/scheme/types"
)

// Lambda implements lambda values.
type Lambda struct {
	Capture *VMEnvFrame
	Impl    *LambdaImpl
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
	return v.Impl.Equal(ov.Impl)
}

// Type implements the Value.Type().
func (v *Lambda) Type() *types.Type {
	t := &types.Type{
		Enum:   types.EnumLambda,
		Return: v.Impl.Return,
	}
	for _, arg := range v.Impl.Args.Fixed {
		if arg.Type == nil {
			t.Args = append(t.Args, types.Any)
		} else {
			t.Args = append(t.Args, arg.Type)
		}
	}
	if v.Impl.Args.Rest != nil {
		if v.Impl.Args.Rest.Type == nil {
			t.Rest = &types.Type{
				Enum: types.EnumPair,
				Car:  types.Unspecified,
				Cdr:  types.Any,
			}
		} else {
			t.Rest = &types.Type{
				Enum: types.EnumPair,
				Car:  v.Impl.Args.Rest.Type,
				Cdr:  types.Any,
			}
		}
	}
	if v.Impl.Parametrizer != nil {
		t.Parametrizer = v.Impl
	}
	return t
}

// Unbox implements Value.Unbox.
func (v *Lambda) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

func (v *Lambda) String() string {
	return v.Impl.Signature(false)
}

// MapPC maps the program counter value to the source location.
func (v *Lambda) MapPC(pc int) (source string, line int) {
	source = v.Impl.Source

	if false {
		fmt.Printf("\nMapPC: %v:%v\n", source, pc)
		for idx, pm := range v.Impl.PCMap {
			fmt.Printf(" - %v\tPC=%v, Line=%v\n", idx, pm.PC, pm.Line)
		}
		v.Impl.Code.Print(os.Stdout)
	}

	line = v.Impl.PCMap.MapPC(pc)
	return
}

// LambdaImpl implements lambda functions.
type LambdaImpl struct {
	Name         string
	Args         Args
	Return       *types.Type
	Captures     bool
	Capture      *VMEnvFrame
	Native       Native
	Parametrizer Parametrize
	Source       string
	Code         Code
	MaxStack     int
	PCMap        PCMap
	Body         []AST
}

// Scheme implements the Value.Scheme().
func (v *LambdaImpl) Scheme() string {
	return v.Signature(false)
}

// Signature prints the lambda signature with optional lambda body.
func (v *LambdaImpl) Signature(body bool) string {
	var str strings.Builder

	str.WriteRune('(')
	if len(v.Name) != 0 {
		str.WriteString(v.Name)
	} else {
		str.WriteString("lambda")
	}
	str.WriteString(" ")

	str.WriteString(v.Args.String())

	if v.Native != nil {
		str.WriteString(" {native}")
	} else if body && len(v.Body) > 0 {
		for _, ast := range v.Body {
			str.WriteRune(' ')
			str.WriteString(fmt.Sprintf("%v", ast))
		}
	} else if v.Code != nil {
		str.WriteString(" {compiled}")
	} else {
		str.WriteString(" ...")
	}
	str.WriteRune(')')
	str.WriteString(v.Return.String())

	return str.String()
}

// Eq implements the Value.Eq().
func (v *LambdaImpl) Eq(o Value) bool {
	return v == o
}

// Equal implements the Value.Equal().
func (v *LambdaImpl) Equal(o Value) bool {
	ov, ok := o.(*LambdaImpl)
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

// Type implements the Value.Type().
func (v *LambdaImpl) Type() *types.Type {
	return types.Unspecified
}

// Unbox implements Value.Unbox.
func (v *LambdaImpl) Unbox() (Value, *types.Type) {
	return v, v.Type()
}

// Parametrize implements types.Parametrizer.Parametrize.
func (v *LambdaImpl) Parametrize(ctx types.Ctx, params []*types.Type) (
	*types.Type, error) {
	return v.Parametrizer(params)
}

// Args specify lambda arguments.
type Args struct {
	Min   int
	Max   int
	Fixed []*TypedName
	Rest  *TypedName
}

// Equal tests if the arguments are equal.
func (args Args) Equal(o Args) bool {
	if args.Min != o.Min || args.Max != o.Max ||
		len(args.Fixed) != len(o.Fixed) {
		return false
	}
	for idx, n := range args.Fixed {
		if n.Name != o.Fixed[idx].Name {
			return false
		}
	}
	if args.Rest == nil {
		if o.Rest != nil {
			return false
		}
	} else if o.Rest == nil {
		return false
	} else if args.Rest.Name != o.Rest.Name {
		return false
	}
	return true
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
		str.WriteString(arg.String())
	}
	if args.Rest != nil {
		str.WriteString(" . ")
		str.WriteString(args.Rest.String())
	}
	str.WriteRune(')')
	return str.String()
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

// TypedName defines name with type information.
type TypedName struct {
	Name string
	Type *types.Type
}

func (tn *TypedName) String() string {
	var result = tn.Name

	if tn.Type != nil {
		result += fmt.Sprintf("<%s>", tn.Type)
	}
	switch tn.Type.Kind {
	case types.Optional:
		return "[" + result + "]"

	case types.Rest:
		return result + "..."

	default:
		return result
	}
}

// Native implements native functions.
type Native func(scm *Scheme, args []Value) (Value, error)

// Parametrize implements argument parametrization.
type Parametrize func(args []*types.Type) (*types.Type, error)

// Builtin defines a built-in native function.
type Builtin struct {
	Name        string
	Aliases     []string
	Args        []string
	Return      *types.Type
	Flags       Flags
	Native      Native
	Parametrize Parametrize
}

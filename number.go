//
// Copyright (c) 2022, 2023-2023 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
	"math"
	"math/big"
)

// Number implements numeric values.
type Number struct {
	Base  int
	Value interface{}
}

// NewNumber creates a new numeric value.
func NewNumber(base int, value interface{}) Number {
	var numValue interface{}

	switch v := value.(type) {
	case int:
		numValue = int64(v)

	case int64:
		numValue = v

	case *big.Int:
		numValue = v

	default:
		panic(fmt.Sprintf("unsupported number: %v(%T)", v, v))
	}
	return Number{
		Base:  base,
		Value: numValue,
	}
}

// Copy creates a new independent copy of the number.
func (n Number) Copy() Number {
	result := Number{
		Base: n.Base,
	}
	switch v := n.Value.(type) {
	case int64:
		result.Value = v

	case *big.Int:
		i := big.NewInt(0)
		result.Value = i.Set(v)

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}

	return result
}

// Const creates a constant value with the same underlying number
// type.
func (n Number) Const(val int64) Number {
	result := Number{
		Base: n.Base,
	}

	switch n.Value.(type) {
	case int64:
		result.Value = val

	case *big.Int:
		result.Value = big.NewInt(val)

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}

	return result
}

// Scheme returns the value as a Scheme string.
func (n Number) Scheme() string {
	return n.String()
}

// Eq tests if the argument value is eq? to this value.
func (n Number) Eq(o Value) bool {
	on, ok := o.(Number)
	if !ok {
		return false
	}

	switch v := n.Value.(type) {
	case int64:
		ov, ok := on.Value.(int64)
		if !ok {
			return false
		}
		return v == ov

	case *big.Int:
		ov, ok := on.Value.(*big.Int)
		if !ok {
			return false
		}
		return v.Cmp(ov) == 0

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

// Equal tests if the argument value is equal to this number.
func (n Number) Equal(o Value) bool {
	on, ok := o.(Number)
	if !ok {
		return false
	}

	switch v := n.Value.(type) {
	case int64:
		switch ov := on.Value.(type) {
		case int64:
			return v == ov

		case *big.Int:
			return ov.Cmp(big.NewInt(v)) == 0

		default:
			panic(fmt.Sprintf("uint64: o type %T not implemented", on.Value))
		}

	case *big.Int:
		switch ov := on.Value.(type) {
		case int64:
			return v.Cmp(big.NewInt(ov)) == 0

		case *big.Int:
			return v.Cmp(ov) == 0

		default:
			panic(fmt.Sprintf("*big.Int: o type %T not implemented", on.Value))
		}

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

// Lt tests if the number is smaller than the argument value.
func (n Number) Lt(o Value) bool {
	on, ok := o.(Number)
	if !ok {
		return false
	}

	switch v := n.Value.(type) {
	case int64:
		switch ov := on.Value.(type) {
		case int64:
			return v < ov

		case *big.Int:
			return big.NewInt(v).Cmp(ov) == -1

		default:
			panic(fmt.Sprintf("uint64: o type %T not implemented", on.Value))
		}

	case *big.Int:
		switch ov := on.Value.(type) {
		case int64:
			return v.Cmp(big.NewInt(ov)) == -1

		case *big.Int:
			return v.Cmp(ov) == -1

		default:
			panic(fmt.Sprintf("*big.Int: o type %T not implemented", on.Value))
		}

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

// Gt tests if the number is smaller than the argument value.
func (n Number) Gt(o Value) bool {
	on, ok := o.(Number)
	if !ok {
		return false
	}

	switch v := n.Value.(type) {
	case int64:
		switch ov := on.Value.(type) {
		case int64:
			return v > ov

		case *big.Int:
			return big.NewInt(v).Cmp(ov) == 1

		default:
			panic(fmt.Sprintf("uint64: o type %T not implemented", on.Value))
		}

	case *big.Int:
		switch ov := on.Value.(type) {
		case int64:
			return v.Cmp(big.NewInt(ov)) == 1

		case *big.Int:
			return v.Cmp(ov) == 1

		default:
			panic(fmt.Sprintf("*big.Int: o type %T not implemented", on.Value))
		}

	default:
		panic(fmt.Sprintf("n type %T not implemented", n.Value))
	}
}

// Int64 returns the number as int64 value.
func (n Number) Int64() int64 {
	switch v := n.Value.(type) {
	case int64:
		return v

	case *big.Int:
		return v.Int64()

	default:
		panic(fmt.Errorf("Number.Int64: invalid number: %v", v))
	}
}

// Bit returns the value of the i'th bit. The bit index must be >= 0.
func (n Number) Bit(i int) uint {
	if i < 0 {
		panic(fmt.Sprintf("Number.Bit: index is negative: %v", i))
	}
	switch v := n.Value.(type) {
	case int64:
		return uint((v >> i) & 0x1)

	case *big.Int:
		return v.Bit(i)

	default:
		panic(fmt.Errorf("Number.Int64: invalid number: %v", v))
	}
}

// Add adds the argument number to this number and returns the sum.
func (n *Number) Add(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = v + o.Int64()

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = v.Int64() + ov

		case *big.Int:
			result.Value = v.Add(v, ov)

		default:
			return n.Const(0), fmt.Errorf("+: unsupport number %v", ov)
		}
	default:
		return n.Const(0), fmt.Errorf("+: unsupport number %v", v)
	}
	return result, nil
}

// Sub subtracts the argument number to this number and returns the
// result.
func (n *Number) Sub(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = v - o.Int64()

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = v.Int64() - ov

		case *big.Int:
			result.Value = v.Sub(v, ov)

		default:
			return n.Const(0), fmt.Errorf("-: unsupport number %v", ov)
		}
	default:
		return n.Const(0), fmt.Errorf("-: unsupport number %v", v)
	}
	return result, nil
}

// Mul multiplies the argument number with this number and returns the
// product.
func (n *Number) Mul(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = v * o.Int64()

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = v.Int64() * ov

		case *big.Int:
			result.Value = v.Mul(v, ov)

		default:
			return n.Const(0), fmt.Errorf("*: unsupport number %v", ov)
		}
	default:
		return n.Const(0), fmt.Errorf("*: unsupport number %v", v)
	}
	return result, nil
}

// Div divides the argument number with this number and returns the
// quotient.
func (n *Number) Div(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = v / o.Int64()

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = v.Int64() / ov

		case *big.Int:
			result.Value = v.Div(v, ov)

		default:
			return n.Const(0), fmt.Errorf("*: unsupport number %v", ov)
		}
	default:
		return n.Const(0), fmt.Errorf("*: unsupport number %v", v)
	}
	return result, nil
}

// Mod returns the modulo of dividing n by o.
func (n *Number) Mod(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = v % o.Int64()

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = v.Int64() % ov

		case *big.Int:
			result.Value = v.Mod(v, ov)

		default:
			return n.Const(0), fmt.Errorf("mod: unsupport number %v", ov)
		}
	default:
		return n.Const(0), fmt.Errorf("mod: unsupport number %v", v)
	}
	return result, nil
}

// Sqrt computes square root of this number.
func (n *Number) Sqrt() (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = int64(math.Sqrt(float64(v)))

	case *big.Int:
		result.Value = v.Sqrt(v)

	default:
		return n.Const(0), fmt.Errorf("expt: invalid number %v", v)
	}
	return result, nil
}

// Expt computes this number to the power of the argument number.
func (n *Number) Expt(o Number) (Number, error) {
	var result Number

	switch v := n.Value.(type) {
	case int64:
		result.Value = int64(math.Pow(float64(v), float64(o.Int64())))

	case *big.Int:
		switch ov := o.Value.(type) {
		case int64:
			result.Value = int64(math.Pow(float64(v.Int64()), float64(ov)))

		case *big.Int:
			result.Value = v.Exp(v, ov, nil)

		default:
			return n.Const(0), fmt.Errorf("expt: invalid number %v", ov)
		}

	default:
		return n.Const(0), fmt.Errorf("expt: invalid number %v", v)
	}
	return result, nil
}

func (n Number) String() string {
	switch v := n.Value.(type) {
	case int64:
		switch n.Base {
		case 2:
			return fmt.Sprintf("#b%b", v)
		case 8:
			return fmt.Sprintf("#o%o", v)
		case 10:
			return fmt.Sprintf("#d%d", v)
		case 16:
			return fmt.Sprintf("#x%x", v)

		default:
			return fmt.Sprintf("%v", n.Value)
		}

	case *big.Int:
		switch n.Base {
		case 2:
			return fmt.Sprintf("#e#b%v", v.Text(n.Base))
		case 8:
			return fmt.Sprintf("#e#o%v", v.Text(n.Base))
		case 10:
			return fmt.Sprintf("#e#d%v", v.Text(n.Base))
		case 16:
			return fmt.Sprintf("#e#x%v", v.Text(n.Base))

		default:
			return fmt.Sprintf("#e%v", v.Text(10))

		}

	default:
		return fmt.Sprintf("{%v[%T]}", n.Value, v)
	}
}

var numberBuiltins = []Builtin{
	{
		Name: "number?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			_, ok := args[0].(Number)
			return Boolean(ok), nil
		},
	},
	{
		Name: "integer?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			n, ok := args[0].(Number)
			if !ok {
				return Boolean(false), nil
			}
			switch n.Value.(type) {
			case int64, *big.Int:
				return Boolean(true), nil
			}
			return Boolean(false), nil
		},
	},
	{
		Name: "exact?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			n, ok := args[0].(Number)
			if !ok {
				return Boolean(false), nil
			}
			switch n.Value.(type) {
			case *big.Int:
				return Boolean(true), nil
			}
			return Boolean(false), nil
		},
	},
	{
		Name: "inexact?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			n, ok := args[0].(Number)
			if !ok {
				return Boolean(false), nil
			}
			switch n.Value.(type) {
			case *big.Int:
				return Boolean(false), nil
			}
			return Boolean(true), nil
		},
	},
	{
		Name: "scheme::=",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return Boolean(z1.Equal(z2)), nil
		},
	},
	{
		Name: "scheme::<",
		Args: []string{"x1", "x2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			x1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			x2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return Boolean(x1.Lt(x2)), nil
		},
	},
	{
		Name: "scheme::>",
		Args: []string{"x1", "x2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			x1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			x2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return Boolean(x1.Gt(x2)), nil
		},
	},
	{
		Name: "odd?",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(z.Bit(0) == 1), nil
		},
	},
	{
		Name: "even?",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(z.Bit(0) == 0), nil
		},
	},
	{
		Name: "scheme::+",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Add(z2)
		},
	},
	{
		Name: "scheme::*",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Mul(z2)
		},
	},
	{
		Name: "scheme::-",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Sub(z2)
		},
	},
	{
		Name: "scheme::/",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Div(z2)
		},
	},
	{
		Name: "mod",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Mod(z2)
		},
	},
	{
		Name: "sqrt",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			return z.Sqrt()
		},
	},
	{
		Name: "expt",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, l.Errorf("invalid argument: %v", args[1])
			}
			return z1.Expt(z2)
		},
	},
}

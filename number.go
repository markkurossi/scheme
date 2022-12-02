//
// Copyright (c) 2022 Markku Rossi
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

// Zero defines an inexact number 0.
var Zero = Number{
	Value: int64(0),
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
			return Zero, fmt.Errorf("+: unsupport number %v", ov)
		}
	default:
		return Zero, fmt.Errorf("+: unsupport number %v", v)
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
			return Zero, fmt.Errorf("-: unsupport number %v", ov)
		}
	default:
		return Zero, fmt.Errorf("-: unsupport number %v", v)
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
			return Zero, fmt.Errorf("*: unsupport number %v", ov)
		}
	default:
		return Zero, fmt.Errorf("*: unsupport number %v", v)
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
			return Zero, fmt.Errorf("expt: invalid number %v", ov)
		}

	default:
		return Zero, fmt.Errorf("expt: invalid number %v", v)
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
		Name: "+",
		Args: []string{"[z1]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			sum := NewNumber(0, big.NewInt(0))
			var err error

			for _, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("+: invalid argument %v", arg)
				}
				sum, err = sum.Add(num)
				if err != nil {
					return nil, err
				}
			}
			return sum, nil
		},
	},
	{
		Name: "-",
		Args: []string{"z1", "[z2]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			result := NewNumber(0, big.NewInt(0))
			var err error

			for idx, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("-: invalid argument %v", arg)
				}
				if idx == 0 && len(args) > 1 {
					result = num
				} else {
					result, err = result.Sub(num)
					if err != nil {
						return nil, err
					}
				}
			}
			return result, nil
		},
	},
	{
		Name: "*",
		Args: []string{"[z1]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			product := NewNumber(0, big.NewInt(1))
			var err error

			for _, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("*: invalid argument %v", arg)
				}
				product, err = product.Mul(num)
				if err != nil {
					return nil, err
				}
			}
			return product, nil
		},
	},
	{
		Name: "=",
		Args: []string{"z1", "z2", "[z1]..."},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			var last Number

			for idx, arg := range args {
				num, ok := arg.(Number)
				if !ok {
					return nil, fmt.Errorf("=: invalid argument %v", arg)
				}
				if idx > 0 && !last.Equal(num) {
					return Boolean(false), nil
				}
				last = num
			}
			return Boolean(true), nil
		},
	},
	{
		Name: "expt",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, args []Value) (Value, error) {
			z1, ok := args[0].(Number)
			if !ok {
				return nil, fmt.Errorf("expt: invalid argument %v", args[0])
			}
			z2, ok := args[1].(Number)
			if !ok {
				return nil, fmt.Errorf("expt: invalid argument %v", args[1])
			}
			return z1.Expt(z2)
		},
	},
}

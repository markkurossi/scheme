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

// NewNumber creates a new numeric value.
func NewNumber(base int, value interface{}) Value {
	switch v := value.(type) {
	case int:
		return Int(v)

	case int64:
		return Int(v)

	case *big.Int:
		return &BigInt{
			I: v,
		}

	default:
		panic(fmt.Sprintf("unsupported number: %v(%T)", v, v))
	}
}

var (
	_ Value = Int(0)
	_ Value = &BigInt{
		I: big.NewInt(0),
	}
)

type Int int64

func (v Int) String() string {
	return fmt.Sprintf("%v", int64(v))
}

func (v Int) Scheme() string {
	return v.String()
}

func (v Int) Eq(o Value) bool {
	ov, ok := o.(Int)
	if !ok {
		return false
	}
	return v == ov
}

func (v Int) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return v == ov

	case *BigInt:
		return ov.I.Cmp(big.NewInt(int64(v))) == 0

	default:
		return false
	}
}

type BigInt struct {
	I *big.Int
}

func (v *BigInt) String() string {
	return v.I.String()
}

func (v *BigInt) Scheme() string {
	return v.String()
}

func (v *BigInt) Eq(o Value) bool {
	ov, ok := o.(*BigInt)
	if !ok {
		return false
	}
	return v.I.Cmp(ov.I) == 0
}

func (v *BigInt) Equal(o Value) bool {
	switch ov := o.(type) {
	case Int:
		return v.I.Cmp(big.NewInt(int64(ov))) == 0

	case *BigInt:
		return v.I.Cmp(ov.I) == 0

	default:
		return false
	}
}

func Int64(v Value) (int64, error) {
	switch val := v.(type) {
	case Int:
		return int64(val), nil

	case *BigInt:
		return val.I.Int64(), nil

	default:
		return 0, fmt.Errorf("invalid number: %v", v)
	}
}

func Add(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 + v2, nil

		case *BigInt:
			return Int(int64(v1) + v2.I.Int64()), nil

		default:
			return Int(0), fmt.Errorf("+: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(v1.I.Int64() + int64(v2)), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Add(v1.I, v2.I),
			}, nil

		default:
			return Int(0), fmt.Errorf("+: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("+: unsupport number: %v", z1)
	}
}

func Sub(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 - v2, nil

		case *BigInt:
			return Int(int64(v1) - v2.I.Int64()), nil

		default:
			return Int(0), fmt.Errorf("-: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(v1.I.Int64() - int64(v2)), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Sub(v1.I, v2.I),
			}, nil

		default:
			return Int(0), fmt.Errorf("-: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("-: unsupport number: %v", z1)
	}
}

func Mul(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 * v2, nil

		case *BigInt:
			return Int(int64(v1) * v2.I.Int64()), nil

		default:
			return Int(0), fmt.Errorf("*: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(v1.I.Int64() * int64(v2)), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Mul(v1.I, v2.I),
			}, nil

		default:
			return Int(0), fmt.Errorf("*: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("*: unsupport number: %v", z1)
	}
}

func Div(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 / v2, nil

		case *BigInt:
			return Int(int64(v1) / v2.I.Int64()), nil

		default:
			return Int(0), fmt.Errorf("/: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(v1.I.Int64() / int64(v2)), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Div(v1.I, v2.I),
			}, nil

		default:
			return Int(0), fmt.Errorf("/: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("/: unsupport number: %v", z1)
	}
}

func Mod(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return v1 % v2, nil

		case *BigInt:
			return Int(int64(v1) % v2.I.Int64()), nil

		default:
			return Int(0), fmt.Errorf("mod: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(v1.I.Int64() % int64(v2)), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Mod(v1.I, v2.I),
			}, nil

		default:
			return Int(0), fmt.Errorf("mod: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("mod: unsupport number: %v", z1)
	}
}

func Sqrt(z Value) (Value, error) {
	switch v := z.(type) {
	case Int:
		return Int(math.Sqrt(float64(v))), nil

	case *BigInt:
		return &BigInt{
			I: new(big.Int).Sqrt(v.I),
		}, nil

	default:
		return Int(0), fmt.Errorf("sqrt: unsupport number: %v", z)
	}
}

func Expt(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return Int(math.Pow(float64(v1), float64(v2))), nil

		case *BigInt:
			return Int(math.Pow(float64(v1), float64(v2.I.Int64()))), nil

		default:
			return Int(0), fmt.Errorf("expt: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Int(math.Pow(float64(v1.I.Int64()), float64(v2))), nil

		case *BigInt:
			return &BigInt{
				I: new(big.Int).Exp(v1.I, v2.I, nil),
			}, nil

		default:
			return Int(0), fmt.Errorf("expt: unsupport number: %v", z2)
		}

	default:
		return Int(0), fmt.Errorf("expt: unsupport number: %v", z1)
	}
}

func Lt(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 < v2), nil

		case *BigInt:
			return Boolean(big.NewInt(int64(v1)).Cmp(v2.I) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("<: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.I.Cmp(big.NewInt(int64(v2))) == -1), nil

		case *BigInt:
			return Boolean(v1.I.Cmp(v2.I) == -1), nil

		default:
			return Boolean(false), fmt.Errorf("<: unsupport number: %v", z2)
		}

	default:
		return Boolean(false), fmt.Errorf("<: unsupport number: %v", z1)
	}
}

func Gt(z1, z2 Value) (Value, error) {
	switch v1 := z1.(type) {
	case Int:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1 > v2), nil

		case *BigInt:
			return Boolean(big.NewInt(int64(v1)).Cmp(v2.I) == 1), nil

		default:
			return Boolean(false), fmt.Errorf(">: unsupport number: %v", z2)
		}

	case *BigInt:
		switch v2 := z2.(type) {
		case Int:
			return Boolean(v1.I.Cmp(big.NewInt(int64(v2))) == 1), nil

		case *BigInt:
			return Boolean(v1.I.Cmp(v2.I) == 1), nil

		default:
			return Boolean(false), fmt.Errorf(">: unsupport number: %v", z2)
		}

	default:
		return Boolean(false), fmt.Errorf(">: unsupport number: %v", z1)
	}
}

func Bit(z Value, i int) (uint, error) {
	switch v := z.(type) {
	case Int:
		return uint((v >> i) & 0x1), nil

	case *BigInt:
		return v.I.Bit(i), nil

	default:
		return 0, fmt.Errorf("bit: unsupport number: %v", z)
	}
}

var numberBuiltins = []Builtin{
	{
		Name: "number?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int, *BigInt:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX complex?
	// XXX real?
	// XXX rational?
	{
		Name: "integer?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int, *BigInt:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX real-valued?
	// XXX rational-valued?
	// XXX integer-valued?
	{
		Name: "exact?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch args[0].(type) {
			case *BigInt:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	{
		Name: "inexact?",
		Args: []string{"obj"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch args[0].(type) {
			case Int:
				return Boolean(true), nil

			default:
				return Boolean(false), nil
			}
		},
	},
	// XXX inexact
	// XXX exact
	{
		Name: "scheme::=",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			switch v1 := args[0].(type) {
			case Int, *BigInt:
				switch v2 := args[1].(type) {
				case Int, *BigInt:
					return Boolean(v1.Equal(v2)), nil
				default:
					return nil, l.Errorf("invalid argument: %v", args[1])
				}
			default:
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
		},
	},
	{
		Name: "scheme::<",
		Args: []string{"x1", "x2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Lt(args[0], args[1])
		},
	},
	{
		Name: "scheme::>",
		Args: []string{"x1", "x2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Gt(args[0], args[1])
		},
	},
	{
		Name: "odd?",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			bit, err := Bit(args[0], 0)
			if err != nil {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(bit == 1), nil
		},
	},
	{
		Name: "even?",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			bit, err := Bit(args[0], 0)
			if err != nil {
				return nil, l.Errorf("invalid argument: %v", args[0])
			}
			return Boolean(bit == 0), nil
		},
	},
	{
		Name: "scheme::+",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Add(args[0], args[1])
		},
	},
	{
		Name: "scheme::*",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Mul(args[0], args[1])
		},
	},
	{
		Name: "scheme::-",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Sub(args[0], args[1])
		},
	},
	{
		Name: "scheme::/",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Div(args[0], args[1])
		},
	},
	{
		Name: "mod",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Mod(args[0], args[1])
		},
	},
	{
		Name: "sqrt",
		Args: []string{"z"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Sqrt(args[0])
		},
	},
	{
		Name: "expt",
		Args: []string{"z1", "z2"},
		Native: func(scm *Scheme, l *Lambda, args []Value) (Value, error) {
			return Expt(args[0], args[1])
		},
	},
}

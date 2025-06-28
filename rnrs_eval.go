//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"

	"github.com/markkurossi/scheme/types"
)

// EvalEnv implements environment for eval.
type EvalEnv struct {
	scm      *Scheme
	bindings map[string]Value
}

// NewEvalEnv creates a new eval environment. The argument scm is
// optional.
func NewEvalEnv(scm *Scheme) *EvalEnv {
	return &EvalEnv{
		scm:      scm,
		bindings: make(map[string]Value),
	}
}

// Set sets the named value in the environment.
func (env *EvalEnv) Set(name string, v Value) {
	env.bindings[name] = v
}

// Get gets the named value from the environment.
func (env *EvalEnv) Get(name string) (Value, bool) {
	v, ok := env.bindings[name]
	if ok {
		return v, ok
	}
	if env.scm == nil {
		return nil, false
	}
	id, ok := env.scm.symbols[name]
	if !ok {
		return nil, false
	}
	return id.Global, true
}

// Eval evaluates the value in the environment.
func Eval(value Value, env *EvalEnv) (Value, error) {
	switch v := value.(type) {
	case *Number:
		unboxed, _ := Unbox(v)
		return unboxed, nil

	case *BigInt, *Bytevector, *Port, *Vector, Boolean, Character, Float, Int,
		String:
		return v, nil

	case *Identifier:
		iv, ok := env.Get(v.Name)
		if !ok {
			return nil, fmt.Errorf("undefined symbol '%v'", v.Name)
		}
		return iv, nil

	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return v, nil
		}
		length := len(list)
		if isKeyword(v.Car(), KwQuote) {
			if length != 2 {
				return nil, v.Errorf("invalid quote: %v", v)
			}
			quoted, _ := Unbox(list[1].Car())
			return quoted, nil
		}
		if isKeyword(v.Car(), KwQuasiquote) {
			if length != 2 {
				return nil, v.Errorf("invalid quasiquote: %v", v)
			}
			quoted, _ := Unbox(list[1].Car())
			return evalQuasiquote(1, quoted, env)
		}
		if isKeyword(v.Car(), KwUnquote) ||
			isKeyword(v.Car(), KwUnquoteSplicing) {
			return v, nil
		}
		return nil, v.Errorf("eval: call not supported: %v", v)

	default:
		return nil, fmt.Errorf("eval: %v[%T] not supported", v, v)
	}
}

func evalQuasiquote(level int, value Value, env *EvalEnv) (Value, error) {
	switch v := value.(type) {
	case Pair:
		list, ok := ListPairs(v)
		if !ok {
			return v, nil
		}
		lb := new(ListBuilder)

	list:
		for _, p := range list {
			item := p.Car()

			lvl, kw, qv := unquote(item)
			if lvl == level {
				switch kw {
				case KwUnquote:
					unquoted, err := Eval(qv, env)
					if err != nil {
						return nil, err
					}
					lb.AddPair(NewLocationPair(p.From(), p.To(), unquoted, nil))

				case KwUnquoteSplicing:
					unquoted, err := Eval(qv, env)
					if err != nil {
						return nil, err
					}
					uqlist, ok := ListPairs(unquoted)
					if !ok {
						// XXX error location
						return nil, p.Errorf("invalid unquote-splicing")
					}
					for _, uqp := range uqlist {
						lb.AddPair(NewLocationPair(uqp.From(), uqp.To(),
							uqp.Car(), nil))
					}
				}
				continue list
			}

			pair, ok := item.(Pair)

			if !ok {
				lb.AddPair(NewLocationPair(p.From(), p.To(), item, nil))
				continue
			}
			ilist, ok := ListPairs(pair)
			if !ok {
				lb.AddPair(NewLocationPair(p.From(), p.To(), item, nil))
				continue
			}

			if isKeyword(pair.Car(), KwQuasiquote) {
				if len(ilist) != 2 {
					return nil, pair.Errorf("invalid quasiquote")
				}
				quoted, _ := Unbox(ilist[1].Car())
				unquoted, err := evalQuasiquote(level+1, quoted, env)
				if err != nil {
					return nil, err
				}
				lb.AddPair(NewLocationPair(p.From(), p.To(), unquoted, nil))
			}
		}
		return lb.B(), nil

	default:
		return nil, fmt.Errorf("eval: invalid quasiquote: %v[%T]", v, v)
	}
}

func unquote(value Value) (int, Keyword, Value) {
	var keyword Keyword

	for level := 0; ; level++ {
		pair, ok := value.(Pair)
		if !ok {
			return level, keyword, value
		}
		next, ok := pair.Cdr().(Pair)
		if !ok || next.Cdr() != nil {
			return level, keyword, value
		}
		car := pair.Car()
		kw, ok := car.(Keyword)
		if !ok {
			return level, keyword, value
		}
		if kw != KwUnquote && kw != KwUnquoteSplicing {
			return level, keyword, value
		}
		keyword = kw
		value = next.Car()
	}
}

var rnrsEvalBuiltins = []Builtin{
	{
		Name:   "interaction-environment",
		Return: types.Pair,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return NewPair(scm.Intern("env"),
				NewPair(scm.Intern("interaction"), nil)), nil
		},
	},
	{
		Name:   "eval",
		Args:   []string{"obj", "obj"},
		Return: types.Any,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			env := NewEvalEnv(scm)
			return Eval(args[0], env)
		},
	},
}

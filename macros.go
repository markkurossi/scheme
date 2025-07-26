//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

func (p *Parser) parseMacro(env *Env, scope MacroScope, list []Pair) (
	AST, error) {

	if len(list) != 3 {
		return nil, list[0].Errorf("%v: expected <keyword> <expression>", scope)
	}
	sym, ok := list[1].Car().(*Symbol)
	if !ok {
		return nil, list[1].Errorf("%v: not an identifier: %v",
			scope, list[1].Car())
	}
	macro := &ASTMacro{
		From:      list[0],
		Scope:     scope,
		Symbol:    sym,
		Literals:  make(map[string]*Symbol),
		Variables: make(map[string]*Symbol),
	}

	expr, ok := ListPairs(list[2].Car())
	if !ok {
		return nil, list[2].Errorf("%v: expression is not a list: %v",
			scope, list[2].Car())
	}
	if len(expr) == 0 {
		return nil, list[2].Errorf("%s: expected symbol", scope)
	}
	kind, ok := expr[0].Car().(*Symbol)
	if !ok {
		return nil, list[2].Errorf("%s: expected symbol: %v",
			scope, expr[0].Car())
	}
	switch kind.Name {
	case "syntax-rules":
		return p.parseSyntaxRules(env, macro, expr)

	case "identifier-syntax":
		return nil, expr[0].Errorf("%v: %v not implemented yet",
			scope, kind.Name)

	default:
		return nil, expr[0].Errorf("%v: invalid macro syntax: %v",
			scope, kind.Name)
	}

}

func (p *Parser) parseSyntaxRules(env *Env, macro *ASTMacro, list []Pair) (
	AST, error) {

	if len(list) < 3 {
		return nil,
			list[0].Errorf("%v: expected (<literal>...) <syntax rule>...",
				list[0].Car())
	}
	ids, ok := ListPairs(list[1].Car())
	if !ok {
		return nil, list[1].Errorf("%v: invalid literals: %v",
			list[0].Car(), list[1].Car())
	}

	macro.Literals[macro.Symbol.Name] = macro.Symbol

	for _, id := range ids {
		sym, ok := id.Car().(*Symbol)
		if !ok {
			return nil, id.Errorf("%v: invalid symbol: %v",
				list[0].Car(), id.Car())
		}
		switch sym.Name {
		case "...", "_":
			return nil, id.Errorf("%v: invalid identifier: %v",
				list[0].Car(), sym)
		default:
			_, ok = macro.Literals[sym.Name]
			if ok {
				return nil, id.Errorf("%v: symbol %v already defined",
					list[0].Car(), sym)
			}
			macro.Literals[sym.Name] = sym
		}
	}

	// Parse all syntax rules.
	for i := 2; i < len(list); i++ {
		parts, ok := ListPairs(list[i].Car())
		if !ok || len(parts) != 2 {
			return nil, list[i].Errorf("%v: expected (<srpattern> <template>)",
				list[0].Car())
		}
		srpattern, err := p.parseSyntaxRule(macro, parts[0].Car())
		if err != nil {
			return nil, parts[0].Errorf("%v: invalid syntax rule pattern: %v",
				list[0].Car(), err)
		}
		paren, ok := srpattern.(*MacroPatternParen)
		if !ok {
			return nil, parts[0].Errorf("%v: invalid syntax rule pattern",
				list[0].Car())
		}
		fmt.Printf("srpattern: %v\n", paren)

		template, err := p.parseTemplate(macro, parts[1].Car(), 0)
		if err != nil {
			return nil, parts[1].Errorf("%v: invalid template: %v",
				list[0].Car(), err)
		}
		fmt.Printf("template : %\n", template)
	}

	return nil, list[0].Errorf("%v not implemented yet", list[0].Car())
}

func (p *Parser) parseSyntaxRule(macro *ASTMacro, value Value) (
	MacroPattern, error) {

	switch v := value.(type) {
	case Pair:
		srp := new(MacroPatternParen)
		list, ok := ListPairs(v)
		if !ok {
			return nil, fmt.Errorf("invalid srpattern")
		}
		if len(list) == 0 {
			return nil, fmt.Errorf("empty syntax rule")
		}
		for _, item := range list {
			pattern, err := p.parseSyntaxRule(macro, item.Car())
			if err != nil {
				return nil, err
			}
			_, ok := pattern.(MacroPatternEllipsis)
			if len(srp.items) > 0 && ok {
				switch prev := srp.items[len(srp.items)-1].(type) {
				case MacroPatternVar, *MacroPatternParen:
					srp.items[len(srp.items)-1] = &MacroPatternMany{
						p: prev,
					}
					continue

				case *MacroPatternMany:
					return nil, fmt.Errorf("unexpected \"...\"")
				}
			}
			srp.items = append(srp.items, pattern)
		}
		switch first := srp.items[0].(type) {
		case MacroPatternID, MacroPatternVar, MacroPatternAny:
		case *MacroPatternMany:
			_, ok := first.p.(MacroPatternVar)
			if !ok {
				return nil, fmt.Errorf("expected an identifier or _: %v", first)
			}
		default:
			return nil, fmt.Errorf("expected an identifier or _: %v", first)
		}
		return srp, nil

	case *Symbol:
		_, ok := macro.Literals[v.Name]
		if ok {
			return MacroPatternID(v.Name), nil
		}
		switch v.Name {
		case "_":
			return MacroPatternAny("_"), nil
		case "...":
			return MacroPatternEllipsis("..."), nil
		default:
			macro.Variables[v.Name] = v
			return MacroPatternVar(v.Name), nil
		}

	default:
		return nil, fmt.Errorf("invalid syntax rule: %v", v)
	}
}

func (p *Parser) parseTemplate(macro *ASTMacro, value Value, nesting int) (
	Value, error) {
	switch v := value.(type) {
	case Pair:
		// XXX
		_ = v
	}
	return nil, fmt.Errorf("parseTemplate not implemented yet")
}

// MacroPattern implements macro patterns.
type MacroPattern interface {
	fmt.Stringer
	Match(tmpl []Pair) ([]Pair, *EvalEnv, bool)
}

var (
	_ MacroPattern = MacroPatternID("n")
	_ MacroPattern = MacroPatternVar("body")
	_ MacroPattern = MacroPatternConst{v: Int(64)}
	_ MacroPattern = MacroPatternAny("_")
	_ MacroPattern = MacroPatternEllipsis("...")
	_ MacroPattern = &MacroPatternParen{}
	_ MacroPattern = &MacroPatternMany{p: MacroPatternVar("body")}
)

// MacroPatternID matches the identifier.
type MacroPatternID string

// Match implements MacroPattern.Match.
func (p MacroPatternID) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	sym, ok := tmpl[0].Car().(*Symbol)
	if !ok || sym.Name != string(p) {
		return tmpl, nil, false
	}
	return tmpl[1:], nil, true
}

func (p MacroPatternID) String() string {
	return string(p)
}

// MacroPatternVar matches one input value and binds the variable with
// the matched value.
type MacroPatternVar string

// Match implements MacroPattern.Match.
func (p MacroPatternVar) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	env := NewEvalEnv(nil)
	env.Set(string(p), tmpl[0].Car())

	return tmpl[1:], env, true
}

func (p MacroPatternVar) String() string {
	return string(p)
}

// MacroPatternConst matches the constant value.
type MacroPatternConst struct {
	v Value
}

// Match implements MacroPattern.Match.
func (p MacroPatternConst) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	if !Equal(p.v, tmpl[0].Car()) {
		return tmpl, nil, false
	}
	return tmpl[1:], nil, false
}

func (p MacroPatternConst) String() string {
	return ToString(p.v)
}

// MacroPatternAny matches one input value.
type MacroPatternAny string

// Match implements MacroPattern.Match.
func (p MacroPatternAny) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	return tmpl[1:], nil, true
}

func (p MacroPatternAny) String() string {
	return "_"
}

// MacroPatternEllipsis matches the "..." symbol.
type MacroPatternEllipsis string

// Match implements MacroPattern.Match.
func (p MacroPatternEllipsis) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	sym, ok := tmpl[0].Car().(*Symbol)
	if !ok || sym.Name != "..." {
		return tmpl, nil, false
	}
	return tmpl[1:], nil, true
}

func (p MacroPatternEllipsis) String() string {
	return "..."
}

// MacroPatternParen matches the parenthesized pattern form.
type MacroPatternParen struct {
	items []MacroPattern
}

// Match implements MacroPattern.Match.
func (p MacroPatternParen) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	if len(tmpl) == 0 {
		return tmpl, nil, false
	}
	list, ok := ListPairs(tmpl[0].Car())
	if !ok {
		return tmpl, nil, false
	}
	t, env, ok := p.match(nil, p.items, list)
	if !ok {
		return tmpl, nil, false
	}
	if len(t) > 0 {
		// Not all parenthesis pattern matched.
		return tmpl, nil, false
	}
	return tmpl[1:], env, true
}

func (p MacroPatternParen) match(env *EvalEnv, pattern []MacroPattern,
	tmpl []Pair) ([]Pair, *EvalEnv, bool) {

	if len(pattern) == 0 {
		return tmpl, env, len(tmpl) == 0
	}

	many, ok := pattern[0].(*MacroPatternMany)
	if ok {
		return p.matchMany(env, many.p, pattern[1:], tmpl)
	}

	if len(tmpl) == 0 {
		return tmpl, nil, false
	}

	var e *EvalEnv

	tmpl, e, ok = pattern[0].Match(tmpl)
	if !ok {
		return tmpl, nil, false
	}
	if env == nil {
		env = e
	} else if e != nil {
		for k, v := range e.bindings {
			_, ok = env.bindings[k]
			if ok {
				// Binding already set.
				return tmpl, nil, false
			}
			env.bindings[k] = v
		}
	}

	return p.match(env, pattern[1:], tmpl)
}

func (p MacroPatternParen) matchMany(env *EvalEnv, many MacroPattern,
	pattern []MacroPattern, tmpl []Pair) ([]Pair, *EvalEnv, bool) {

	var ok bool
	t := tmpl

	for i := 0; i <= len(tmpl); i++ {
		t, env, ok = p.match(env, pattern, t)
		if ok {
			v, ok := many.(MacroPatternVar)
			if ok {
				if env == nil {
					env = NewEvalEnv(nil)
				}
				var result ListBuilder
				for j := 0; j < i; j++ {
					result.AddPair(DerivePair(tmpl[j], tmpl[j].Car(), nil))
				}
				env.Set(string(v), result.Head)
			}
			return t, env, ok
		}

		// Try one more match of `many'.
		t, env, ok = many.Match(t)
		if !ok {
			break
		}
	}
	return t, nil, len(pattern) == 0 && len(tmpl) == 0
}

func (p MacroPatternParen) String() string {
	result := "("
	for idx, item := range p.items {
		if idx > 0 {
			result += " "
		}
		result += item.String()
	}
	return result + ")"
}

// MacroPatternMany matches zero or more repetitions of the wrapped
// pattern.
type MacroPatternMany struct {
	p MacroPattern
}

// Match implements MacroPattern.Match.
func (p MacroPatternMany) Match(tmpl []Pair) ([]Pair, *EvalEnv, bool) {
	return tmpl[1:], nil, true
}

func (p MacroPatternMany) String() string {
	return p.p.String() + " ..."
}

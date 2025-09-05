//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"fmt"
)

// Macro implements scheme macros.
type Macro struct {
	From        Locator
	Scope       MacroScope
	Kind        MacroKind
	Symbol      *Symbol
	Literals    map[string]*Symbol
	Variables   map[string]*Symbol
	SyntaxRules []*SyntaxRule
	nextSymbol  int
}

// NextSymbol returns the next unused symbol name for the macro.
func (macro *Macro) NextSymbol() string {
	for {
		name := fmt.Sprintf("_var%v", macro.nextSymbol)
		macro.nextSymbol++

		if macro.Symbol.Name == name {
			continue
		}
		_, ok := macro.Literals[name]
		if ok {
			continue
		}
		_, ok = macro.Variables[name]
		if ok {
			continue
		}
		return name
	}
}

// MacroScope defines macro scopes.
type MacroScope int

// Macro scopes.
const (
	MacroDefine MacroScope = iota
	MacroLet
	MacroLetrec
)

var macroScopeNames = map[MacroScope]string{
	MacroDefine: "define-syntax",
	MacroLet:    "let-syntax",
	MacroLetrec: "letrec-syntax",
}

func (scope MacroScope) String() string {
	name, ok := macroScopeNames[scope]
	if ok {
		return name
	}
	panic("unknown MacroScope")
}

// MacroKind specifies the macro type.
type MacroKind int

// Macro types.
const (
	MacroSyntaxRules MacroKind = iota
	MacroIdentifierSyntax
	MacroSyntaxCase
)

// SyntaxRule implements macro syntax rules.
type SyntaxRule struct {
	Pattern      MacroPattern
	Template     Value
	SubTemplates []*SubTemplate
}

// Expand expands the syntax rule with the values from env.
func (sr *SyntaxRule) Expand(env *EvalEnv) (Value, error) {
	// Expand all sub templates.
	for _, tmpl := range sr.SubTemplates {
		expanded, err := tmpl.Expand(env)
		if err != nil {
			return nil, err
		}
		env.Set(tmpl.Name, expanded)
	}
	return Eval(sr.Template, env)
}

// SubTemplate implements a parenthized sub-template.
type SubTemplate struct {
	Name      string
	Template  Value
	Variables map[string]*Symbol
}

// Expand expands the sub-template with the values from env.
func (tmpl *SubTemplate) Expand(env *EvalEnv) (Value, error) {
	vars := make(map[string][]Pair)
	length := -1

	for k := range tmpl.Variables {
		envValue, ok := env.Get(k)
		if !ok {
			return nil, fmt.Errorf("undefined symbol '%v'", k)
		}
		pairs, ok := ListPairs(envValue)
		if !ok {
			return nil, fmt.Errorf("invalid value for symbol '%v'", k)
		}
		if length < 0 {
			length = len(pairs)
		} else if length != len(pairs) {
			return nil, fmt.Errorf("different value length for symbol '%v'", k)
		}
		vars[k] = pairs
	}

	// Expand template for all values.
	var result ListBuilder
	for i := 0; i < length; i++ {
		e := NewEvalEnv(nil)
		for k := range tmpl.Variables {
			e.Set(k, vars[k][i].Car())
		}
		expanded, err := Eval(tmpl.Template, e)
		if err != nil {
			return nil, err
		}
		result.Add(expanded)
	}

	return result.B(), nil
}

// Match matches the macro with the value.
func (macro *Macro) Match(v Value) (*SyntaxRule, *EvalEnv) {
	var pair Pair
	locator, ok := v.(Locator)
	if ok {
		pair = NewLocationPair(locator.From(), locator.To(), v, nil)
	} else {
		pair = NewPair(v, nil)
	}
	input := []Pair{
		pair,
	}
	for _, rule := range macro.SyntaxRules {
		_, env, ok := rule.Pattern.Match(input)
		if ok {
			return rule, env
		}
	}
	return nil, nil
}

func (p *Parser) macroExpand(value Value) (Value, bool, error) {
	var count int

	locator, locatorOK := value.(Locator)

	for ; count < 1000; count++ {
		switch v := value.(type) {
		case *Symbol:
			return v, false, nil

		case Pair:
			sym, ok := v.Car().(*Symbol)
			if ok {
				macro, ok := p.scm.macros[sym.Name]
				if ok {
					rule, env := macro.Match(v)
					if rule == nil {
						return nil, false,
							v.Errorf("%s: no matching syntax rule", sym)
					}
					expanded, err := rule.Expand(env)
					if err != nil {
						return nil, false, v.Errorf("%v", err)
					}
					value = expanded
					continue
				}
				switch sym.Name {
				case "define-syntax":
					list, ok := ListPairs(v)
					if !ok {
						return v, false, nil
					}
					if len(list) == 0 {
						return v, false, nil
					}
					macro, err := p.parseMacro(MacroDefine, list)
					if err != nil {
						return nil, false, v.Errorf("%v", err)
					}
					_, ok = p.scm.macros[macro.Symbol.Name]
					if ok {
						return nil, false, list[1].Errorf("macro %v redefined",
							macro.Symbol.Name)
					}
					p.scm.macros[macro.Symbol.Name] = macro
					return nil, true, nil

				default:
				}
			}

			for v != nil {
				// Expand car.
				expanded, skip, err := p.macroExpand(v.Car())
				if err != nil {
					return nil, false, v.Errorf("%v", err)
				}
				if skip {
					return nil, false, v.Errorf("syntax error")
				}
				v.SetCar(expanded)

				// Handle cdr by its type.
				switch cdr := v.Cdr().(type) {
				case Pair:
					v = cdr

				default:
					expanded, skip, err := p.macroExpand(cdr)
					if err != nil {
						return nil, false, v.Errorf("%v", err)
					}
					if skip {
						return nil, false, v.Errorf("syntax error")
					}
					v.SetCdr(expanded)
					v = nil
				}
			}
			return value, false, nil

		default:
			return value, false, nil
		}
	}

	if locatorOK {
		return nil, false,
			locator.From().Errorf("macro expansion loop (tried %v times)",
				count)
	}
	return nil, false, fmt.Errorf("macro expansion loop (tried %v times)",
		count)
}

func (p *Parser) parseMacro(scope MacroScope, list []Pair) (*Macro, error) {
	if len(list) != 3 {
		return nil, list[0].Errorf("%v: expected <keyword> <expression>", scope)
	}
	sym, ok := list[1].Car().(*Symbol)
	if !ok {
		return nil, list[1].Errorf("%v: not an identifier: %v",
			scope, list[1].Car())
	}
	macro := &Macro{
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
		return macro.parseSyntaxRules(expr)

	case "identifier-syntax":
		return nil, expr[0].Errorf("%v: %v not implemented yet",
			scope, kind.Name)

	default:
		return nil, expr[0].Errorf("%v: invalid macro syntax: %v",
			scope, kind.Name)
	}
}

func (macro *Macro) parseSyntaxRules(list []Pair) (*Macro, error) {

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

		srpattern, err := macro.parseSyntaxRule(parts[0].Car())
		if err != nil {
			return nil, parts[0].Errorf("%v: invalid syntax rule pattern: %v",
				list[0].Car(), err)
		}
		_, ok = srpattern.(*MacroPatternParen)
		if !ok {
			return nil, parts[0].Errorf("%v: invalid syntax rule pattern",
				list[0].Car())
		}

		sr := &SyntaxRule{
			Pattern: srpattern,
		}

		template, err := macro.parseTemplate(sr, parts[1].Car(), 0, false, nil)
		if err != nil {
			return nil, parts[1].Errorf("%v: invalid template: %v",
				list[0].Car(), err)
		}
		sr.Template = template

		macro.SyntaxRules = append(macro.SyntaxRules, sr)
	}

	return macro, nil
}

func (macro *Macro) parseSyntaxRule(value Value) (MacroPattern, error) {

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
			pattern, err := macro.parseSyntaxRule(item.Car())
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
			switch first.p.(type) {
			case MacroPatternVar, *MacroPatternParen:
			default:
				return nil,
					fmt.Errorf("expected an identifier, pattern, or _: %v",
						first.p)
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

	case String:
		return MacroPatternConst{v: v}, nil

	default:
		return nil, fmt.Errorf("invalid syntax rule: %v", v)
	}
}

func (macro *Macro) parseTemplate(sr *SyntaxRule, value Value, nesting int,
	splicing bool, vars map[string]*Symbol) (Value, error) {

	switch v := value.(type) {
	case Pair:
		nesting++
		head := v

		var result ListBuilder

		for v != nil {
			this := v.Car()

			var splicing bool

			next, ok := v.Cdr().(Pair)
			if ok && IsSymbol(next.Car(), "...") {
				splicing = true
				value = next.Cdr()
			} else {
				value = v.Cdr()
			}

			subTemplate, ok := this.(Pair)
			if splicing && ok {
				sub := &SubTemplate{
					Name: macro.NextSymbol(),
				}
				subVars := make(map[string]*Symbol)
				parsed, err := macro.parseTemplate(sr, subTemplate, 0, false,
					subVars)
				if err != nil {
					return nil, err
				}
				sub.Template = parsed
				sub.Variables = subVars
				sr.SubTemplates = append(sr.SubTemplates, sub)

				sym := NewSymbol(sub.Name)
				sym.Point = subTemplate.From()

				result.AddPair(DerivePair(v, macro.uq(sym, nesting, splicing),
					nil))
			} else {
				car, err := macro.parseTemplate(sr, this, nesting, splicing,
					vars)
				if err != nil {
					return nil, err
				}
				result.AddPair(DerivePair(v, car, nil))
			}

			switch cdr := value.(type) {
			case Pair:
				v = cdr
			case nil:
				v = nil
			default:
				cdrv, err := macro.parseTemplate(sr, cdr, nesting, false, vars)
				if err != nil {
					return nil, err
				}
				result.Tail.SetCdr(cdrv)
				v = nil
			}
		}
		var qq ListBuilder
		qq.AddPair(DerivePair(head, NewSymbol("quasiquote"), nil))
		qq.AddPair(DerivePair(head, result.B(), nil))

		return qq.B(), nil

	case *Symbol:
		_, ok := macro.Variables[v.Name]
		if !ok {
			return v, nil
		}
		if vars != nil {
			vars[v.Name] = v
		}
		return macro.uq(v, nesting, splicing), nil

	default:
		return v, nil
	}
}

func (macro *Macro) uq(v *Symbol, nesting int, splicing bool) Value {
	var result Value = v

	for i := 0; i < nesting; i++ {
		var uq ListBuilder
		if i == 0 && splicing {
			uq.AddPair(NewLocationPair(v.Point, v.Point,
				NewSymbol("unquote-splicing"), nil))
		} else {
			uq.AddPair(NewLocationPair(v.Point, v.Point,
				NewSymbol("unquote"), nil))
		}
		uq.AddPair(NewLocationPair(v.Point, v.Point, result, nil))
		result = uq.B()
	}
	return result
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
	if len(tmpl) == 0 {
		return tmpl, nil, false
	}
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
	return tmpl[1:], nil, true
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

	var accu map[string]*ListBuilder

	switch many.(type) {
	case MacroPatternVar:
	case *MacroPatternParen:
		accu = make(map[string]*ListBuilder)
	default:
		panic(fmt.Sprintf("invalid MacroPatternMany element: %T", many))
	}

	for i := 0; i <= len(tmpl); i++ {
		t, env, ok = p.match(env, pattern, t)
		if ok {
			if env == nil {
				env = NewEvalEnv(nil)
			}

			switch v := many.(type) {
			case MacroPatternVar:
				var result ListBuilder
				for j := 0; j < i; j++ {
					result.AddPair(DerivePair(tmpl[j], tmpl[j].Car(), nil))
				}
				env.Set(string(v), result.B())

			case *MacroPatternParen:
				for k, v := range accu {
					env.Set(k, v.B())
				}

			default:
				panic("invalid MacroPatternMany element")
			}
			return t, env, ok
		}

		// Try one more match of `many'.
		var e *EvalEnv
		t, e, ok = many.Match(t)
		if !ok {
			break
		}
		if accu != nil {
			for k, v := range e.bindings {
				builder, ok := accu[k]
				if !ok {
					builder = new(ListBuilder)
					accu[k] = builder
				}
				builder.Add(v)
			}
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

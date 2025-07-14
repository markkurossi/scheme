//
// Copyright (c) 2022-2025 Markku Rossi
//
// All rights reserved.
//

package scheme

import (
	"errors"
	"fmt"
	"strings"

	"github.com/markkurossi/scheme/types"
)

var (
	_ Pair = &PlainPair{}
	_ Pair = &LocationPair{}
)

// Pair implements a Scheme pair.
type Pair interface {
	Locator
	Car() Value
	Cdr() Value
	SetCar(v Value) error
	SetCdr(v Value) error
	Scheme() string
	Eq(o Value) bool
	Equal(o Value) bool
	Type() *types.Type
	Unbox() (Value, *types.Type)
}

// DerivePair creates a new pair with the argument car and cdr
// values. The function derives any metadata (location information)
// from the pair from.
func DerivePair(from Pair, car, cdr Value) Pair {
	switch p := from.(type) {
	case *PlainPair:
		return NewPair(car, cdr)
	case *LocationPair:
		return NewLocationPair(p.from, p.to, car, cdr)
	default:
		panic("invalid pair")
	}
}

// List creates a list from the argument values.
func List(values ...Value) Value {
	var head, tail Pair
	for _, v := range values {
		pair := NewPair(v, nil)
		if head == nil {
			head = pair
		} else {
			tail.SetCdr(pair)
		}
		tail = pair
	}

	return head
}

// ListBuilder builds scheme lists.
type ListBuilder struct {
	Head Pair
	Tail Pair
}

// B returns the list.
func (lb *ListBuilder) B() Value {
	return lb.Head
}

// Add appends the argument values to the list.
func (lb *ListBuilder) Add(values ...Value) *ListBuilder {
	for _, v := range values {
		lb.AddPair(NewPair(v, nil))
	}
	return lb
}

// AddPair appends the argument pair to the list.
func (lb *ListBuilder) AddPair(p Pair) *ListBuilder {
	if lb.Head == nil {
		lb.Head = p
	} else {
		lb.Tail.SetCdr(p)
	}
	lb.Tail = p

	return lb
}

// PatchLocation updates the value's location information with the
// origin. The location information is updated only for pairs with
// empty Point.Source value.
func PatchLocation(value Value, origin Point) {
	pair, ok := value.(Pair)
	if !ok {
		return
	}
	if len(pair.From().Source) > 0 {
		return
	}
	pair.SetFrom(origin)
	pair.SetTo(origin)

	PatchLocation(pair.Car(), origin)
	PatchLocation(pair.Cdr(), origin)
}

// PlainPair implements a Scheme pair with car and cdr values.
type PlainPair struct {
	car Value
	cdr Value
}

// NewPair creates a new pair with the car and cdr values.
func NewPair(car, cdr Value) Pair {
	return &PlainPair{
		car: car,
		cdr: cdr,
	}
}

// From returns pair's start location.
func (pair *PlainPair) From() Point {
	return Point{}
}

// To returns pair's end location.
func (pair *PlainPair) To() Point {
	return Point{}
}

// SetFrom implements Locator.SetFrom.
func (pair *PlainPair) SetFrom(p Point) {
}

// SetTo implements Locator.SetTo.
func (pair *PlainPair) SetTo(p Point) {
}

// Errorf implements Locator.Errorf.
func (pair *PlainPair) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		return fmt.Errorf("%s", msg)
	}
	return fmt.Errorf("\u2260 %s", msg)
}

// Warningf implements Locator.Warningf.
func (pair *PlainPair) Warningf(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("warning: %s", msg)
	} else {
		fmt.Printf("\u2260 warning: %s", msg)
	}
}

// Infof implements Locator.Infof.
func (pair *PlainPair) Infof(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("%s", msg)
	} else {
		fmt.Printf("\u2260 %s", msg)
	}
}

// Car returns the pair's car value.
func (pair *PlainPair) Car() Value {
	return pair.car
}

// Cdr returns the pair's cdr value.
func (pair *PlainPair) Cdr() Value {
	return pair.cdr
}

// SetCar sets the pair's car value.
func (pair *PlainPair) SetCar(v Value) error {
	pair.car = v
	return nil
}

// SetCdr sets the pair's cdr value.
func (pair *PlainPair) SetCdr(v Value) error {
	pair.cdr = v
	return nil
}

// Scheme returns the value as a Scheme string.
func (pair *PlainPair) Scheme() string {
	return pair.String()
}

// Eq tests if the argument value is eq? to this value.
func (pair *PlainPair) Eq(o Value) bool {
	ov, ok := o.(*PlainPair)
	return ok && pair == ov
}

// Equal tests if the argument value is equal to this value.
func (pair *PlainPair) Equal(o Value) bool {
	ov, ok := o.(Pair)
	return ok && Equal(pair.car, ov.Car()) && Equal(pair.cdr, ov.Cdr())
}

// Type implements the Value.Type().
func (pair *PlainPair) Type() *types.Type {
	var t *types.Type

	err := Map(func(idx int, v Value) error {
		if v == nil {
			t = types.Unify(t, types.Nil)
		} else {
			t = types.Unify(t, v.Type())
		}
		return nil
	}, pair)
	if err == nil {
		return &types.Type{
			Enum: types.EnumPair,
			Car:  t,
			Cdr:  types.Unspecified,
		}
	}

	t = &types.Type{
		Enum: types.EnumPair,
		Car:  types.Unspecified,
		Cdr:  types.Unspecified,
	}
	if pair.car != nil {
		t.Car = pair.car.Type()
	}

	return t
}

// Unbox implements Value.Unbox.
func (pair *PlainPair) Unbox() (Value, *types.Type) {
	result := &PlainPair{
		car: pair.car,
		cdr: pair.cdr,
	}
	if result.car != nil {
		v, _ := result.car.Unbox()
		result.car = v
	}
	if result.cdr != nil {
		v, _ := result.cdr.Unbox()
		result.cdr = v
	}
	return result, pair.Type()
}

func (pair *PlainPair) String() string {
	var str strings.Builder
	str.WriteRune('(')

	i := Pair(pair)
	var count int
	turtle := pair

loop:
	for {
		if count > 0 {
			str.WriteRune(' ')
		}
		count++

		if i.Car() == nil {
			str.WriteString("nil")
		} else {
			str.WriteString(i.Car().Scheme())
		}
		switch cdr := i.Cdr().(type) {
		case Pair:
			i = cdr

		case nil:
			break loop

		default:
			str.WriteString(" . ")
			str.WriteString(cdr.Scheme())
			break loop
		}
		if i == turtle {
			str.WriteString(" . \u221E")
			break loop
		}

		if count%2 == 0 {
			cdr, ok := turtle.Cdr().(*PlainPair)
			if ok {
				turtle = cdr
			}
		}
	}
	str.WriteRune(')')

	return str.String()
}

// LocationPair implements a Scheme pair with location information.
type LocationPair struct {
	from Point
	to   Point
	PlainPair
}

// NewLocationPair creates a new pair with the car and cdr values and
// location information.
func NewLocationPair(from, to Point, car, cdr Value) Pair {
	return &LocationPair{
		from: from,
		to:   to,
		PlainPair: PlainPair{
			car: car,
			cdr: cdr,
		},
	}
}

// From returns pair's start location.
func (pair *LocationPair) From() Point {
	return pair.from
}

// To returns pair's end location.
func (pair *LocationPair) To() Point {
	return pair.to
}

// SetFrom implements Locator.SetFrom.
func (pair *LocationPair) SetFrom(p Point) {
	pair.from = p
}

// SetTo implements Locator.SetTo.
func (pair *LocationPair) SetTo(p Point) {
	pair.to = p
}

// Eq tests if the argument value is eq? to this value.
func (pair *LocationPair) Eq(o Value) bool {
	ov, ok := o.(*LocationPair)
	return ok && pair == ov
}

// Errorf implements Locator.Errorf.
func (pair *LocationPair) Errorf(format string, a ...interface{}) error {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		return fmt.Errorf("%s: %s", pair.from, msg)
	}
	return fmt.Errorf("%s: \u2260 %s", pair.from, msg)
}

// Warningf implements Locator.Warningf.
func (pair *LocationPair) Warningf(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("%s: warning: %s", pair.from, msg)
	} else {
		fmt.Printf("%s: \u2260 warning: %s", pair.from, msg)
	}
}

// Infof implements Locator.Infof.
func (pair *LocationPair) Infof(format string, a ...interface{}) {
	msg := fmt.Sprintf(format, a...)
	if isTagged(msg) {
		fmt.Printf("%s: %s", pair.from, msg)
	} else {
		fmt.Printf("%s: \u2260 %s", pair.from, msg)
	}
}

func (pair *LocationPair) String() string {
	return pair.PlainPair.String()
}

// ErrorInvalidList is used to indicate when a malformed or otherwise
// invalid list is passed to list functions.
var ErrorInvalidList = errors.New("invalid list")

// ListLength check if the argument value is a valid Scheme list.
func ListLength(list Value) (int, bool) {
	var count int

	err := Map(func(idx int, v Value) error { count++; return nil }, list)
	if err != nil {
		return 0, false
	}
	return count, true
}

// ListPairs returns the list pairs as []Pair.
func ListPairs(list Value) ([]Pair, bool) {
	var result []Pair

	err := MapPairs(func(idx int, p Pair) error {
		result = append(result, p)
		return nil
	}, list)
	if err != nil {
		return nil, false
	}
	return result, true
}

// ListValues returns the list values as []Value.
func ListValues(list Value) ([]Value, bool) {
	var result []Value

	err := Map(func(idx int, v Value) error {
		result = append(result, v)
		return nil
	}, list)
	if err != nil {
		return nil, false
	}
	return result, true
}

// Map maps function for each element of the list. The function
// returns nil if the argument list is a list and map functions
// returns nil for each of its element.
func Map(f func(idx int, v Value) error, list Value) error {
	return MapPairs(func(idx int, p Pair) error {
		return f(idx, p.Car())
	}, list)
}

// MapPairs maps function for each pair of the list. The function
// returns nil if the argument list is a list and map functions
// returns nil for each of its element.
func MapPairs(f func(idx int, p Pair) error, list Value) error {
	if list == nil {
		return nil
	}
	pair, ok := list.(Pair)
	if !ok {
		return ErrorInvalidList
	}

	var turtle Pair

	for idx := 0; pair != nil; idx++ {
		if err := f(idx, pair); err != nil {
			point := pair.From()
			if point.Undefined() {
				return err
			}
			return fmt.Errorf("%s: %v", point, err)
		}
		if pair == turtle {
			return ErrorInvalidList
		}
		if idx%2 == 0 {
			if turtle == nil {
				turtle = pair
			} else {
				switch cdr := turtle.Cdr().(type) {
				case Pair:
					turtle = cdr

				case nil:
					pair = nil

				default:
					return ErrorInvalidList
				}
			}
		}
		switch cdr := pair.Cdr().(type) {
		case Pair:
			pair = cdr

		case nil:
			pair = nil

		default:
			return ErrorInvalidList
		}
	}
	return nil
}

// Car returns the car element of the pair.
func Car(pair Value, ok bool) (Value, bool) {
	if !ok {
		return pair, false
	}
	p, ok := pair.(Pair)
	if !ok {
		return pair, false
	}
	return p.Car(), true
}

// Cdr returns the cdr element of the cons cell.
func Cdr(pair Value, ok bool) (Value, bool) {
	if !ok {
		return pair, false
	}
	p, ok := pair.(Pair)
	if !ok {
		return pair, false
	}
	return p.Cdr(), true
}

var listBuiltins = []Builtin{
	{
		Name:   "pair?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			_, ok := args[0].(Pair)
			return Boolean(ok), nil
		},
	},
	{
		Name: "cons",
		Args: []string{"obj1", "obj2"},
		Return: &types.Type{
			Enum: types.EnumPair,
			Car:  types.Unspecified,
			Cdr:  types.Unspecified,
		},
		Flags: FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return NewPair(args[0], args[1]), nil
		},
		Parametrize: func(args []*types.Type) (*types.Type, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf(
					"invalid amount of arguments: got %v, expected 2",
					len(args))
			}
			return &types.Type{
				Enum: types.EnumPair,
				Car:  args[0],
				Cdr:  args[1],
			}, nil
		},
	},
	{
		Name:   "car",
		Args:   []string{"pair"},
		Return: types.Unspecified,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			pair, ok := args[0].(Pair)
			if !ok {
				return nil, fmt.Errorf("not a pair: %v", args[0])
			}
			return pair.Car(), nil
		},
	},
	{
		Name:   "cdr",
		Args:   []string{"pair"},
		Return: types.Unspecified,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			pair, ok := args[0].(Pair)
			if !ok {
				return nil, fmt.Errorf("not a pair: %v", args[0])
			}
			return pair.Cdr(), nil
		},
	},
	{
		Name:   "null?",
		Args:   []string{"obj"},
		Return: types.Boolean,
		Flags:  FlagConst,
		Native: func(scm *Scheme, args []Value) (Value, error) {
			return Boolean(args[0] == nil), nil
		},
	},
}

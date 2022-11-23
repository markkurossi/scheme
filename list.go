//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"errors"
)

// ErrorInvalidList is used to indicate when a malformed or otherwise
// invalid list is passed to list functions.
var ErrorInvalidList = errors.New("invalid list")

// ListLength check if the argument value is a valid Scheme list.
func ListLength(list Value) (int, bool) {
	var count int

	err := Map(func(v Value) error { count++; return nil }, list)
	if err != nil {
		return 0, false
	}
	return count, true
}

// Map maps function for each element of the list. The function
// returns nil if the argument list is a list and map functions
// returns nil for each of its element.
func Map(f func(v Value) error, list Value) error {
	if list == nil {
		return nil
	}
	cons, ok := list.(*Cons)
	if !ok {
		return ErrorInvalidList
	}

	for cons != nil {
		if err := f(cons.Car); err != nil {
			return err
		}
		switch cdr := cons.Cdr.(type) {
		case *Cons:
			cons = cdr

		case nil:
			cons = nil

		default:
			return ErrorInvalidList
		}
	}
	return nil
}

// Car returns the car element of the cons cell.
func Car(consCell Value, ok bool) (Value, bool) {
	if !ok {
		return consCell, false
	}
	cons, ok := consCell.(*Cons)
	if !ok {
		return consCell, false
	}
	return cons.Car, true
}

// Cdr returns the cdr element of the cons cell.
func Cdr(consCell Value, ok bool) (Value, bool) {
	if !ok {
		return consCell, false
	}
	cons, ok := consCell.(*Cons)
	if !ok {
		return consCell, false
	}
	return cons.Cdr, true
}

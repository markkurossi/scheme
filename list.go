//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

// ListLength check if the argument cons cell is a valid Scheme list.
func ListLength(cons *Cons) (int, bool) {
	var count int

	for cons != nil {
		count++
		switch cdr := cons.Cdr.(type) {
		case *Cons:
			cons = cdr

		case nil:
			cons = nil

		default:
			return 0, false
		}
	}
	return count, true
}

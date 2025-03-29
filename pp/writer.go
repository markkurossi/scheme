//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

// Package pp implements Scheme pretty-printing.
package pp

// Writer implements Scheme pretty-printer.
type Writer interface {
	// Header creates the output header. This is called at the
	// beginning of the pretty-print.
	Header()

	// Trailer creates the output trailer. This is called at the end
	// of the pretty-print.
	Trailer()

	// Push pushes the current column as new left margin.
	Push()

	// Pop removes the current left margin and sets the previous left
	// margin as the current one.
	Pop()

	// PushN adjust the left margin with n columns.
	PushN(n int)

	// Println formats using the default formats for its operands and
	// outputs the result. Spaces are always added between operands
	// and a newline is appended.
	Println(a ...any)

	// Printf prints a formatted message.
	Printf(format string, a ...any)

	// Keyword prints a Scheme keyword.
	Keyword(keyword string)

	// Name prints a Scheme name.
	Name(name string)

	// Type prints a type.
	Type(t string)

	// Literal prints a string literal.
	Literal(t string)

	// Error returns an error if any of the methods failed during the
	// pretty-printing.
	Error() error
}

var (
	_ Writer = &HTML{}
)

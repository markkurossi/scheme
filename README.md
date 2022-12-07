# R4RS Scheme in Go

This Scheme implementation provides R4RS compliant scheme written in
Go. The system has a compiler that translates the scheme program into
a byte code and a virtual machine that executes the code. It will
implement all necessary Scheme features, including tail calls and
continuations.

The compiler implements a simple API for processing S-expressions. The
API gives a clean and high-level abstraction, for example,
configuration file parsing, data encoding and decoding, and other
structured data operations.

# TODO

 - [ ] Compiler
   - [ ] 4.2.1. Conditionals
     - [ ] cond
     - [ ] case
     - [ ] and
     - [ ] or
   - [ ] 4.2.4. Iteration
     - [ ] do
     - [ ] Named let
   - [ ] 4.2.5. Delayed evaluation
     - [ ] delay
   - [ ] 4.2.6. Quasiquotation
     - [ ] quasiquote
 - [ ] VM
   - [ ] Call with current continuation
 - [ ] Runtime
   - [ ] 6.3. Pairs and lists
     - [ ] append
     - [ ] memq, memv, member
     - [ ] assq, assv, assoc
   - [ ] 6.4. Symbols
     - [ ] symbol?
     - [ ] symbol->string
     - [ ] string->symbol
   - [ ] 6.5.5. Numerical operations
     - [ ] complex?
     - [ ] real?
     - [ ] rational?
     - [ ] integer?
     - [ ] exact?
     - [ ] inexact?
     - [ ] <, >, <=, >=
     - [ ] positive?
     - [ ] negative?
     - [ ] odd?
     - [ ] even?
     - [ ] max?
     - [ ] min?
     - [ ] -, /
     - [ ] abs
     - [ ] quotient
     - [ ] remainder
     - [ ] modulo
     - [ ] gcm
     - [ ] lcm
     - [ ] numerator
     - [ ] denominator
     - [ ] floor
     - [ ] ceiling
     - [ ] truncate
     - [ ] floor
     - [ ] rationalize
     - [ ] exp
     - [ ] log
     - [ ] sin
     - [ ] cos
     - [ ] tan
     - [ ] asin
     - [ ] acos
     - [ ] atan
     - [ ] sqrt
     - [ ] expt
     - [ ] make-rectangular
     - [ ] make-polar
     - [ ] imag-part
     - [ ] magnitude
     - [ ] angle
     - [ ] exact->inexact
     - [ ] inexact->exact
   - [ ] 6.5.6. Numerical input and output
     - [ ] number->string
     - [ ] string->number
   - [ ] 6.7. Strings
	 - [ ] string-set!
	 - [ ] string=?
	 - [ ] string-ci=?
	 - [ ] string<?
	 - [ ] string>?
	 - [ ] string<=?
	 - [ ] string>=?
	 - [ ] string-ci<?
	 - [ ] string-ci>?
	 - [ ] string-ci<=?
	 - [ ] string-ci>=?
	 - [ ] substring
	 - [ ] string-fill!
   - [ ] 6.9. Control features
     - [ ] procedure?
     - [ ] (apply proc arg1 ... args)
     - [ ] map
     - [ ] for-each
     - [ ] force
   - [ ] 6.10.1. Ports
     - [ ] call-with-input-file
     - [ ] call-with-output-file
     - [ ] input-port?
     - [ ] output-port?
     - [ ] current-input-port
     - [ ] current-output-port
     - [ ] with-input-from-file
     - [ ] with-output-to-file
     - [ ] open-input-file
     - [ ] open-output-file
     - [ ] close-input-port
     - [ ] close-output-port
   - [ ] 6.10.2. Input
     - [ ] read
     - [ ] read-char
     - [ ] peek-char
     - [ ] eof-object?
     - [ ] char-ready?
   - [ ] 6.10.3. Output
     - [ ] write
     - [ ] write-char
   - [ ] 6.10.4. System interface
     - [ ] load
     - [ ] transcript-on
     - [ ] transcript-off

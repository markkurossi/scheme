# R6RS Scheme in Go

This Scheme implementation provides R6RS compliant scheme written in
Go. The system has a compiler that translates the scheme program into
a byte code and a virtual machine that executes the code. It will
implement all necessary Scheme features, including tail calls and
continuations.

The compiler implements a simple API for processing S-expressions. The
API gives a clean and high-level abstraction, for example,
configuration file parsing, data encoding and decoding, and other
structured data operations.

# TODO

 - [ ] API
   - [ ] Marshal / unmarshal
   - [ ] Access to global symbols
 - [ ] Compiler
   - [ ] 4.2.4. Iteration
     - [ ] do
     - [ ] Named let
   - [ ] 4.2.5. Delayed evaluation
     - [ ] delay
   - [ ] 4.2.6. Quasiquotation
     - [ ] quasiquote
 - [ ] VM
   - [ ] Error handlers
   - [ ] Call with current continuation
 - [ ] Base Library `(rnrs base (6))`
   - [ ] 11.7. Arithmetic
     - [ ] complex?
     - [ ] real?
     - [ ] rational?
     - [ ] real-valued?
     - [ ] rational-valued?
     - [ ] integer-valued?
     - [ ] inexact
     - [ ] exact
     - [ ] finite?
     - [ ] infinite?
     - [ ] nan?
     - [ ] abs
     - [ ] div-and-mod
     - [ ] div
     - [ ] div0-and-mod0
     - [ ] div0
     - [ ] mod0
     - [ ] gcm
     - [ ] lcm
     - [ ] numerator
     - [ ] denominator
     - [ ] floor
     - [ ] ceiling
     - [ ] truncate
     - [ ] round
     - [ ] rationalize
     - [ ] exp
     - [ ] log
     - [ ] sin
     - [ ] cos
     - [ ] tan
     - [ ] asin
     - [ ] acos
     - [ ] atan
     - [X] sqrt
     - [ ] exact-integer-sqrt
     - [ ] make-rectangular
     - [ ] make-polar
     - [ ] real-part
     - [ ] imag-part
     - [ ] magnitude
     - [ ] angle
     - [ ] number->string
     - [ ] string->number
   - [ ] 11.12. Strings XXX continue todo update from here
	 - [ ] string-set!
	 - [ ] string-ci=?
	 - [ ] string-ci<?
	 - [ ] string-ci>?
	 - [ ] string-ci<=?
	 - [ ] string-ci>=?
	 - [ ] string-fill!
   - [ ] 6.9. Control features
     - [ ] force
   - [ ] 6.10.1. Ports
     - [ ] call-with-input-file
     - [ ] call-with-output-file
     - [ ] current-input-port
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
     - [ ] transcript-on
     - [ ] transcript-off

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

## Virtual Machine

### Stack

```
sp --->
        Value{n}
        ...
        Value{0}
        Arg{n}
        ...
        Arg{0}
fp ---> Next fp ---+
                   |
                   v
```

## Loading and Evaluation

```
EvalFile(file)
  Eval(source)
    library := Load(sourc)
    Apply(scheme::init-library, []Value{library})
      (importer lib-imports)
      (lib-init) =>
      ((scheme::compile))
```

# TODO

## Language

### Types

```
Any
  |
  +-- Boolean
  |
  +-- String
  |
  +-- Character
  |
  +-- Symbol
  |
  +-- Vector
  |
  +-- Bytevector
  |
  +-- Number
  |     |
  |     +-- ExactInteger (big.Int)
  |     |     |
  |     |     +-- InxactInteger (int64)
  |     |
  |     +-- ExactFloat (big.Float)
  |           |
  |           +-- InxactFloat (float64)
  |
  +-- Lambda(Type...) Type
  |
  +-- Pair(Type, Type)
```

## Misc

 - [ ] Shortlist
   - [ ] Top-levels with `(import)`
   - [ ] Library local symbols
 - [ ] API
   - [ ] Marshal / unmarshal
 - [ ] VM
   - [ ] Tail-call within same function as jump
   - [ ] 5.8. Multiple return values
   - [ ] Error handlers
   - [ ] Call with current continuation
 - [ ] Compiler
   - [ ] 7.1. Library form
     - [ ] import/export
     - [ ] rename
     - [ ] xxx
   - [ ] 8. Top-level programs
   - [ ] 9. Primitive syntax
     - [ ] 9.2. Macros
   - [ ] 10. Expansion process
 - [ ] 11. Base Library `(rnrs base (6))`
   - [ ] 11.2.2. Syntax definitions
     - [ ] define-syntax
   - [ ] 11.3. Bodies
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
   - [ ] 11.16. Iteration
     - [ ] Named let
   - [ ] 11.17. Quasiquotation
     - [ ] quasiquote
     - [ ] unquote
     - [ ] unquote-splicing
 - [ ] R6RS Libraries
   - [ ] 1. Unicode `(rnrs unicode (6))`
     - [ ] char-foldcase
     - [ ] char-general-category
     - [ ] string-titlecase
     - [ ] string-foldcase
     - [ ] string-normalize-nfd
     - [ ] string-normalize-nfkd
     - [ ] string-normalize-nfc
     - [ ] string-normalize-nfkc
   - [ ] 2. Bytevectors `(rnrs bytevectors (6))`
     - [ ] endianness
     - [ ] native-endianness
     - [ ] bytevector-u8-set!
     - [ ] bytevector-s8-set!
     - [ ] bytevector->u8-list
     - [ ] u8-list->bytevector
     - [ ] xxx
   - [ ] 3. List utilities `(rnrs lists (6))`
     - [X] find
     - [ ] for-all
     - [ ] exists
     - [X] filter
     - [ ] partition
     - [ ] fold-left
     - [ ] fold-right
     - [X] remp
     - [X] remove
     - [X] remv
     - [X] remq
     - [ ] cons*
   - [ ] 4. Sorting `(rnrs sorting (6))`
     - [ ] list-sort
     - [ ] vector-sort
     - [ ] vector-sort!
   - [ ] 5. Control structures `(rnrs control (6))`
      - [ ] when
      - [ ] unless
      - [ ] do
      - [ ] case-lambda
   - [ ] 6. Records
   - [ ] 7. Exceptions and conditions
   - [ ] 8. I/O
     - [ ] 8.2. Port I/O `(rnrs io ports (6))`
     - [ ] 8.3. Simple I/O `(rnrs io simple (6))`
       - [ ] eof-object
       - [ ] eof-object?
       - [ ] call-with-input-file
       - [ ] call-with-output-file
       - [ ] current-input-port
       - [ ] with-input-from-file
       - [ ] with-output-to-file
       - [ ] open-input-file
       - [ ] open-output-file
       - [ ] close-input-port
       - [ ] close-output-port
       - [ ] read-char
       - [ ] peek-char
       - [ ] read
       - [ ] write-char
   - [X] 9. File system `(rnrs files (6))`
   - [X] 10. Command-line access and exit values `(rnrs programs (6))`
   - [ ] 11. Arithmetic
   - [ ] 12. syntax-case `(rnrs syntax-case (6))`
   - [ ] 13. Hashtables `(rnrs hashtables (6))`
   - [ ] 14. Enumerations `(rnrs enums (6))`
   - [ ] 15. Composite library `(rnrs (6))`
   - [ ] 16. Eval `(rnrs eval (6))`
   - [X] 17. Mutable pairs `(rnrs mutable-pairs (6))`
   - [ ] 18. Mutable strings `(rnrs mutable-strings (6))`
	 - [ ] string-set!
	 - [ ] string-fill!
   - [ ] 19. R5RS compatibility `(rnrs r5rs (6))`
     - [ ] exact->inexact
     - [ ] inexact->exact
     - [ ] quotient
     - [ ] remainder
     - [X] modulo
     - [ ] delay
     - [ ] force
     - [ ] null-environment
     - [ ] scheme-report-environment

# R6RS Scheme in Go

This Scheme implementation provides mostly R6RS compliant scheme
written in Go. The system has a compiler that translates the scheme
program into a byte code and a virtual machine that executes the
code. It will implement all necessary Scheme features, including tail
calls and continuations.

The compiler implements a simple API for processing S-expressions. The
API gives a clean and high-level abstraction, for example,
configuration file parsing, data encoding and decoding, and other
structured data operations.

## Language

The implementation follows the R6RS specification but makes the
following non-compliant design decisions:
 - The language is type aware and the compiler uses type inference to
   decide types for all expressions. The root of the type tree is
   `any` which is the supertype of all types. Please, see the type
   tree definition below.
 - Numeric tower:
   - Has exact and inexact integer and floating point types. The exact
     numbers use Go's `big.Int` and `big.Float` types and inexact
     numbers use `int64` and `float64` respectively
   - Operations between exact and inexact numbers generate converts to
     exact values (int64 + big.Int = big.Int), when the R6RS specifies
     that the result should be inexact.
 - Global definitions are final and can't be redefined. However, it is
   possible to set their values if the new values are type-compatible
   with the variable definition. You can assing values of same type or
   subtype to a global variable.
 - The `define-constant` syntax defines constant variables which can't
   be redefined.
 - Several unary (`pair?`, `null?`, `zero?`, `car`, `cdr`, `not`) and
   binary (`cons`, `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`)
   functions are inlined and implemented as VM bytecode operands. The
   runtime also implements these funtions as procedures so it is
   possible to `apply` them to arguments.

### Extensions

The `pragma` directive can be used to control Scheme's compilation and
runtime semantics. The `pragma` directives are specified in the
following format:

``` scheme
(pragma (directive1 value1) ...)
```

The following directives are defined:
 - `verbose-typecheck bool` controls type inference verbosity.
 - `check-boolean-exprs bool` controls checking expression boolean
   value in boolean contexts (`if`, `cond`, `and`, `or`)

### Types

The compiler uses type inference to resolve types for all
expressions. The type hierarchy is as follows:

```
Any
  |
  +-- Nil
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
  |     |     +-- InexactInteger (int64)
  |     |
  |     +-- ExactFloat (big.Float)
  |           |
  |           +-- InexactFloat (float64)
  |
  +-- Port
  |
  +-- Lambda(Type...) Type
  |
  +-- Pair(Type, Type)
```

# TODO

 - [ ] Shortlist
   - [x] Top-levels with `(import)`
   - [ ] Library local symbols
 - [ ] API
   - [ ] Marshal / unmarshal
 - [ ] VM
   - [ ] Tail-call within same function as jump
   - [ ] 5.8. Multiple return values
   - [x] Error handlers
   - [ ] Call with current continuation
 - [ ] Compiler
   - [ ] Instantiate lambdas with different numeric argument values
         (inexact float, inexact int)
   - [ ] 7.1. Library form
     - [ ] import/export
     - [ ] rename
     - [ ] xxx
   - [ ] 8. Top-level programs
   - [ ] 9. Primitive syntax
     - [x] 9.2. Macros
   - [ ] 10. Expansion process
 - [ ] 11. Base Library `(rnrs base (6))`
   - [ ] 11.2.2. Syntax definitions
     - [x] define-syntax
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
     - [x] abs
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
     - [x] sqrt
     - [ ] exact-integer-sqrt
     - [ ] make-rectangular
     - [ ] make-polar
     - [ ] real-part
     - [ ] imag-part
     - [ ] magnitude
     - [ ] angle
     - [x] number->string
     - [x] string->number
   - [x] 11.16. Iteration
     - [x] Named let
   - [ ] 11.17. Quasiquotation
     - [ ] quasiquote
     - [ ] unquote
     - [ ] unquote-splicing
 - [ ] R6RS Libraries
   - [ ] 1. Unicode `(rnrs unicode (6))`
     - [ ] char-foldcase
     - [ ] char-general-category
     - [x] string-titlecase
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
     - [X] list-sort
     - [X] vector-sort
     - [ ] vector-sort!
   - [ ] 5. Control structures `(rnrs control (6))`
      - [x] when
      - [x] unless
      - [x] do
      - [ ] case-lambda
   - [ ] 6. Records
   - [ ] 7. Exceptions and conditions
     - [ ] 7.1. Exceptions
       - [x] with-exception-handler
       - [x] guard
       - [x] raise
       - [ ] raise-continuable
     - [ ] 7.2. Conditions
     - [ ] 7.3. Standard condition types
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
   - [x] 16. Eval `(rnrs eval (6))`
   - [X] 17. Mutable pairs `(rnrs mutable-pairs (6))`
   - [ ] 18. Mutable strings `(rnrs mutable-strings (6))`
	 - [ ] string-set!
	 - [ ] string-fill!
   - [ ] 19. R5RS compatibility `(rnrs r5rs (6))`
     - [ ] exact->inexact
     - [ ] inexact->exact
     - [ ] quotient
     - [x] remainder
     - [x] modulo
     - [ ] delay
     - [ ] force
     - [ ] null-environment
     - [ ] scheme-report-environment

Test VM dispatch techniques:

``` go
func (vm *VirtualMachine) execute(p *Program) (eval, error) {
	code := p.code
	ip := 0

	for ip < len(code) {
		ip += code[ip](vm)
		if vm.err != nil {
			return nil, vm.err
		}
	}
	if vm.sp == 0 {
		return nil, nil
	}
	return vm.stack[vm.sp-1], nil
}

func (c *compiler) emitPushNull() {
	c.emit(func(vm *VirtualMachine) int {
		vm.stack[vm.sp] = nil
		vm.sp++
		return 1
	})
}
```

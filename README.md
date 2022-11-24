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
   - [ ] let syntax
   - [ ] Lambda environment capture
   - [ ] Store lexical locations into values
 - [ ] VM
   - Tail call
   - Call with current continuation
 - [ ] Runtime
   - [ ] Almost everything

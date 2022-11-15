;;; WebAssembly binary format.

(bytes
 (msb32 #x0061736d #x01000000)
 20 30 #d42 #o077 #b1010
 #f #t "Hello, \\ \0\n \"world!\""
 #\space #\ #\newline #\x40)

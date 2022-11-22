;;; Hello, world!

(display "The length of \"Hello, world!\" is ")
(display (string-length "Hello, world!"))
(display ".")
(newline)

(define (print msg)
  (display msg)
  (newline))

(print "Hello, lambda!")
(print "Hello, world!")

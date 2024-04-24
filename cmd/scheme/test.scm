(import (go format))

(pragma (verbose-typecheck #t))

(define (foo l base)
  (number->string l base))

(define x (lambda () 42))
(define y (lambda () "foo"))

(foo (type 1) 10)
;;(foo y)

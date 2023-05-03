(define (fib n)
  (if (not (scheme::> n 1))
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(display (fib 33))
;;(display (fib 30))
(newline)
;;(exit)

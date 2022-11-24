;;; Hello, world!

;; (display "The length of \"Hello, world!\" is ")
;; (display (string-length "Hello, world!"))
;; (display ".")
;; (newline)
;;
(define (print msg)
  (display msg)
  (newline))

(print "Hello, lambda!")
(print "Hello, world!")
;;
;; (define msg "Hello, msg!")
;; (set! msg "Hello, set!")
;;
;; (print msg)

(define (say msg)
  ((lambda (y)
     (display y)
     (newline))
   msg))

(say "Hello, world!")

(lambda (msg)
  (display msg)
  (newline))

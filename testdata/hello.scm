;;; Hello, world!

;; (display "The length of \"Hello, world!\" is ")
;; (display (string-length "Hello, world!"))
;; (display ".")
;; (newline)
;;
;; (define (print msg)
;;   (display msg)
;;   (newline))
;;
;; (print "Hello, lambda!")
;; (print "Hello, world!")
;;
;; (define msg "Hello, msg!")
;; (set! msg "Hello, set!")
;;
;; (print msg)

(define (say-maker header msg trailer)
  (lambda (pre post)
    (display header)
    (display pre)
    (display msg)
    (display post)
    (display trailer)
    (newline)))

(define a (say-maker "<html>" "Hello, a!" "</html>"))
(define b (say-maker "<div>" "Hello, b!" "</div>"))

(a "(" ")")
(b "{" "}")

;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (any->scheme obj)
  (cond
   ((null? obj) "'()")
   ((boolean? obj) (if obj "#t" "#f"))
   ((number? obj) (number->string obj))
   ((char? obj) (string-append "#\\" (list->string (cons obj '()))))

   ((string? obj)
    (letrec ((head '())
             (tail '())
             (add
              (lambda (ch)
                (let ((p (cons ch '())))
                  (if (null? tail)
                      (set! head p)
                      (set-cdr! tail p))
                  (set! tail p))))
             (iter
              (lambda (chars)
                (cond
                 ((null? chars)
                  (add #\")
                  (list->string head))
                 ((char=? (car chars) #\\)
                  (add #\\) (add #\\)
                  (iter (cdr chars)))
                 ((char=? (car chars) #\")
                  (add #\\) (add #\")
                  (iter (cdr chars)))
                 (else
                  (add (car chars))
                  (iter (cdr chars)))))))
      (add #\")
      (iter (string->list obj))))

   ((symbol? obj) (symbol->string obj))

   ((pair? obj)
    (letrec ((head '())
             (tail '())
             (turtle '())
             (add
              (lambda (item last)
                (let ((p (cons item '())))
                  (cond
                   ((null? tail)
                    (set! head (cons "(" p)))
                   (last
                    (set-cdr! tail p))
                   (else
                    (set-cdr! tail (cons " " p))))
                  (set! tail p))))
             (iter
              (lambda (even rest)
                (cond
                 ((null? rest)
                  (add ")" #t)
                  (apply string-append head))
                 ((eq? turtle rest)
                  (add "{cycle})" #t)
                  (apply string-append head))
                 ((pair? rest)
                  (if even
                      (if (null? turtle)
                          (set! turtle rest)
                          (set! turtle (cdr turtle))))
                  (add (any->scheme (car rest)) #f)
                  (iter (not even) (cdr rest)))
                 (else
                  (add "." #f)
                  (add (any->scheme rest) #f)
                  (add ")" #t)
                  (apply string-append head))))))
      (iter #t obj)))

   ((vector? obj)
    (letrec ((result "#(")
             (iter
              (lambda (idx)
                (if (>= idx (vector-length obj))
                    (string-append result ")")
                    (begin
                      (if (> idx 0)
                          (set! result (string-append result " ")))
                      (set! result
                            (string-append result
                                           (any->scheme (vector-ref obj idx))))
                      (iter (+ idx 1)))))))
      (iter 0)))

   ((bytevector? obj)
    (letrec ((result "#vu8(")
             (iter
              (lambda (idx)
                (if (>= idx (bytevector-length obj))
                    (string-append result ")")
                    (begin
                      (if (> idx 0)
                          (set! result (string-append result " ")))
                      (set! result
                            (string-append result
                                           (number->string
                                            (bytevector-u8-ref obj idx))))
                      (iter (+ idx 1)))))))
      (iter 0)))

   ;; XXX procedure
   (else "")))

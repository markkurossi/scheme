;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (go format (1 19))
  (export format)
  (import (rnrs base))

  (define (format format . args)
    (letrec ((head '())
             (tail '())

             (char->string
              (lambda (ch)
                (list->string (cons ch '()))))
             (missing-argument
              (lambda (escape)
                (add (string-append "{missing argument for %"
                                    (char->string escape)
                                    "}"))))

             (add
              (lambda (str)
                (let ((p (cons str '())))
                  (if (null? tail)
                      (set! head p)
                      (set-cdr! tail p))
                  (set! tail p))))

             (format-arg
              (lambda (f chars args)
                (cond
                 ((null? args)
                  (missing-argument (car chars))
                  (iter (cdr chars) args))
                 (else
                  (add (f (car args)))
                  (iter (cdr chars) (cdr args))))))

             (result
              (lambda ()
                (apply string-append head)))

             (%-escape
              (lambda (chars args)
                (if (null? chars)
                    (result)
                    (case (car chars)
                      ((#\d)
                       (format-arg number->string chars args))
                      ((#\%)
                       (add "%")
                       (iter (cdr chars) args))
                      (else
                       (add (string-append "{unknown escape %"
                                           (char->string (car chars))
                                           "}"))
                       (iter (cdr chars) args))))))

             (iter
              (lambda (chars args)
                (cond
                 ((null? chars)
                  (result))
                 ((char=? (car chars) #\%)
                  (%-escape (cdr chars) args))
                 (else
                  (add (list->string (list (car chars))))
                  (iter (cdr chars) args))))))
      (iter (string->list format) args)))
  )

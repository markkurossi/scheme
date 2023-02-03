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
             (escape-string
              (lambda (str)
                (apply string-append
                       (append '("\"")
                               (map (lambda (ch)
                                      (case ch
                                       ((#\") "\\\"")
                                       ((#\\) "\\\\")
                                       (else (char->string ch))))
                                    (string->list str))
                               '("\"")))))

             (missing-argument
              (lambda (escape)
                (add (string-append "%!" (char->string escape) "(MISSING)"))))

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
                      ((#\%)
                       (add "%")
                       (iter (cdr chars) args))
                      ((#\t)
                       (format-arg (lambda (arg)
                                     (if arg "true" "false"))
                                   chars args))
                      ((#\b)
                       (format-arg (lambda (arg)
                                     (number->string arg 2))
                                   chars args))
                      ((#\c)
                       (format-arg (lambda (arg)
                                     (list->string (cons (integer->char arg)
                                                         '())))
                                   chars args))
                      ((#\d)
                       (format-arg number->string chars args))
                      ((#\o)
                       (format-arg (lambda (arg)
                                     (number->string arg 8))
                                   chars args))
                      ((#\O)
                       (format-arg (lambda (arg)
                                     (string-append "0o"
                                                    (number->string arg 8)))
                                   chars args))
                      ((#\q)
                       (format-arg
                        (lambda (arg)
                          (cond
                           ((integer? arg)
                            (string-append
                             "#\\" (char->string (integer->char arg))))
                           ((string? arg)
                            (escape-string arg))
                           (else
                            "#!q(UNSUPPORTED)")))
                        chars args))
                      ((#\x)
                       (format-arg (lambda (arg)
                                     (number->string arg 16))
                                   chars args))
                      ((#\X)
                       (format-arg (lambda (arg)
                                     (string-upcase (number->string arg 16)))
                                   chars args))
                      (else
                       (add (string-append "%!" (char->string (car chars))
                                           "(UNKNOWN)"))
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

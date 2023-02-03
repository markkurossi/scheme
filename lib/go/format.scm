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
                (add (string-append "%!" (char->string escape) "(MISSING)"))))

             (add
              (lambda (str)
                (let ((p (cons str '())))
                  (if (null? tail)
                      (set! head p)
                      (set-cdr! tail p))
                  (set! tail p))))

             (make-padding
              (lambda (accu pad len)
                (if (<= len 0)
                    accu
                    (make-padding (string-append accu pad) pad (- len 1)))))

             (format-arg
              (lambda (right-align padding width f chars args)
                (cond
                 ((null? args)
                  (missing-argument (car chars))
                  (iter (cdr chars) args))
                 (else
                  (let* ((str (f (car args)))
                         (len (string-length str)))
                    (if (> width len)
                        (let ((pad (make-padding "" (char->string padding)
                                                 (- width len))))
                          (if right-align
                              (add (string-append pad str))
                              (add (string-append str pad))))
                        (add str))
                    (iter (cdr chars) (cdr args)))))))

             (result
              (lambda (args)
                (if (not (null? args))
                    (begin
                      (add "%!(EXTRA")
                      (for-each (lambda (item)
                                  (add " ")
                                  (add (scheme::->scheme item)))
                                args)
                      (add ")")))
                (apply string-append head)))

             (%-escape
              (lambda (right-align padding width chars args)
                (cond
                 ((null? chars)
                  (result args))
                 ((eq? (car chars) #\-)
                  (%-padding #f padding width (cdr chars) args))
                 (else
                  (%-padding right-align padding width chars args)))))

             (%-padding
              (lambda (right-align padding width chars args)
                (cond
                 ((null? chars)
                  (result args))
                 ((eq? (car chars) #\0)
                  (%-width right-align #\0 width (cdr chars) args))
                 (else
                  (%-width right-align padding width chars args)))))

             (%-width
              (lambda (right-align padding width chars args)
                (cond
                 ((null? chars)
                  (result args))
                 ((and (char<=? #\0 (car chars))
                       (char<=? (car chars) #\9))
                  (%-width right-align padding
                           (+ (* width 10)
                              (- (char->integer (car chars))
                                 (char->integer #\0)))
                           (cdr chars) args))
                 (else
                  (%-verbs right-align padding width chars args)))))

             (%-verbs
              (lambda (right-align padding width chars args)
                (if (null? chars)
                    (result args)
                    (case (car chars)
                      ((#\%)
                       (add "%")
                       (iter (cdr chars) args))
                      ((#\t)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (if arg "true" "false"))
                                   chars args))
                      ((#\b)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (number->string arg 2))
                                   chars args))
                      ((#\c)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (list->string (cons (integer->char arg)
                                                         '())))
                                   chars args))
                      ((#\d)
                       (format-arg right-align padding width
                                   number->string chars args))
                      ((#\o)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (number->string arg 8))
                                   chars args))
                      ((#\O)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (string-append "0o"
                                                    (number->string arg 8)))
                                   chars args))
                      ((#\q)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (cond
                                      ((integer? arg)
                                       (scheme::->scheme (integer->char arg)))
                                      (else
                                       (scheme::->scheme arg))))
                                   chars args))
                      ((#\x)
                       (format-arg right-align padding width
                                   (lambda (arg)
                                     (number->string arg 16))
                                   chars args))
                      ((#\X)
                       (format-arg right-align padding width
                                   (lambda (arg)
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
                  (result args))
                 ((char=? (car chars) #\%)
                  (%-escape #t #\space 0 (cdr chars) args))
                 (else
                  (add (list->string (list (car chars))))
                  (iter (cdr chars) args))))))
      (iter (string->list format) args)))
  )

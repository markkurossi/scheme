;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (string=? str1 str2 . rest)
  (scheme::compare (lambda (x y) (scheme::string=? x y)) str1 str2 rest))

(define (string<? str1 str2 . rest)
  (scheme::compare (lambda (x y) (scheme::string<? x y)) str1 str2 rest))

(define (string>? str1 str2 . rest)
  (scheme::compare (lambda (x y) (scheme::string>? x y)) str1 str2 rest))

(define (string<=? str1 str2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::string>? x y))) str1 str2 rest))

(define (string>=? str1 str2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::string<? x y))) str1 str2 rest))

(define (string-for-each proc . strings)
  (letrec ((s->l
            (lambda (head tail strings)
              (if (null? strings)
                  head
                  (let ((p (cons (string->list (car strings)) '())))
                    (if (null? tail)
                        (set! head p)
                        (set-cdr! tail p))
                    (set! tail p)
                    (s->l head tail (cdr strings)))))))
    (apply for-each (cons proc (s->l '() '() strings)))))

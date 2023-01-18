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

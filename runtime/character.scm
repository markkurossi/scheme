;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (char=? char1 char2 . rest)
  (scheme::compare scheme::char=? char1 char2 rest))

(define (char<? char1 char2 . rest)
  (scheme::compare scheme::char<? char1 char2 rest))

(define (char>? char1 char2 . rest)
  (scheme::compare scheme::char>? char1 char2 rest))

(define (char<=? char1 char2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::char>? x y))) char1 char2 rest))

(define (char>=? char1 char2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::char<? x y))) char1 char2 rest))

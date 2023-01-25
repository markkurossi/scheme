;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (rnrs unicode (6))
  (export char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?)
  (import (rnrs))

  (define (char-ci=? char1 char2 . rest)
    (scheme::compare (lambda (x y)
                       (scheme::char=? (char-upcase x) (char-upcase y)))
                     char1 char2 rest))

  (define (char-ci<? char1 char2 . rest)
    (scheme::compare (lambda (x y)
                       (scheme::char<? (char-upcase x) (char-upcase y)))
                     char1 char2 rest))

  (define (char-ci>? char1 char2 . rest)
    (scheme::compare (lambda (x y)
                       (scheme::char>? (char-upcase x) (char-upcase y)))
                     char1 char2 rest))
  (define (char-ci<=? char1 char2 . rest)
    (scheme::compare (lambda (x y)
                       (not (scheme::char>? (char-upcase x) (char-upcase y))))
                     char1 char2 rest))
  (define (char-ci>=? char1 char2 . rest)
    (scheme::compare (lambda (x y)
                       (not (scheme::char<? (char-upcase x) (char-upcase y))))
                     char1 char2 rest))
  )

;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs unicode library.
;;;

(library (rnrs-unicode-test)
  (export)
  (import (rnrs unicode))

  (runner 'sub-section "1. Unicode")

  (runner 'test "char-ci=?"
          (lambda () (char-ci=? #\a #\A))
          (lambda () (char-ci=? #\a #\A #\A))
          (lambda () (not (char-ci=? #\a #\A #\A #\b)))
          )
  (runner 'test "char-ci<?"
          (lambda () (char-ci<? #\a #\B))
          (lambda () (char-ci<? #\a #\B #\c))
          (lambda () (not (char-ci<? #\a #\B #\C #\c)))
          )
  (runner 'test "char-ci>?"
          (lambda () (char-ci>? #\B #\a))
          (lambda () (char-ci>? #\c #\B #\a))
          (lambda () (not (char-ci>? #\c #\C #\B #\a)))
          )
  (runner 'test "char-ci<=?"
          (lambda () (char-ci<=? #\a #\B))
          (lambda () (char-ci<=? #\a #\B #\b))
          (lambda () (not (char-ci<=? #\a #\B #\C #\b)))
          )
  (runner 'test "char-ci>=?"
          (lambda () (char-ci>=? #\B #\a))
          (lambda () (char-ci>=? #\c #\B #\b))
          (lambda () (not (char-ci>=? #\c #\C #\B #\c)))
          )
  )

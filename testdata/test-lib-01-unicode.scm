;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs unicode library.
;;;

(library (test rnrs unicode)
  (export)
  (import (rnrs unicode))

  (runner 'sub-section "1. Unicode")
  (runner 'sub-sub-section "1.1. Characters")

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

  (runner 'sub-sub-section "1.2. Strings")

  (runner 'test "string-upcase"
          (lambda () (string=? (string-upcase "Hi") "HI"))
          (lambda () (string=? (string-upcase "Straße") "STRAßE"))
          )
  (runner 'test "string-downcase"
          (lambda () (string=? (string-downcase "Hi") "hi"))
          (lambda () (string=? (string-downcase "Straße") "straße"))
          )
  (runner 'test "string-titlecase"
          (lambda () (equal? (string-titlecase "kNock KNoCK") "Knock Knock"))
          ;;(lambda () (equal? (string-titlecase "who's there?") "Who's There?"))
          )

  (runner 'test "string-ci=?"
          (lambda () (string-ci=? "Straße" "straße"))
          (lambda () (string-ci=? "Straße" "STRAßE"))
          (lambda () (string-ci=? "Straße" "STRAßE" "straße"))
          )
  (runner 'test "string-ci<?"
          (lambda () (string-ci<? "Straße" "ttraße"))
          (lambda () (string-ci<? "Straße" "ttraße" "uTRAßE"))
          (lambda () (not (string-ci<? "Straße" "ttraße" "uTRAßE" "uTRAßE")))
          )
  (runner 'test "string-ci>?"
          (lambda () (string-ci>? "ttraße" "Straße"))
          (lambda () (string-ci>? "uTRAßE" "ttraße" "Straße"))
          (lambda () (not (string-ci>? "uTRAßE" "ttraße" "Straße" "Straße")))
          )
  (runner 'test "string-ci<=?"
          (lambda () (string-ci<=? "Straße" "ttraße"))
          (lambda () (string-ci<=? "Straße" "ttraße" "tTRAßE"))
          (lambda () (not (string-ci<=? "Straße" "ttraße" "uTRAßE" "tTRAßE")))
          )
  (runner 'test "string-ci>=?"
          (lambda () (string-ci>=? "ttraße" "Straße"))
          (lambda () (string-ci>=? "uTRAßE" "ttraße" "ttraße"))
          (lambda () (not (string-ci>=? "uTRAßE" "ttraße" "Straße" "ttraße")))
          )
  )

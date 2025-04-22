;;;
;;; Copyright (c) 2023-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the Go language library.
;;;

(library (main)
  (export)
  (import (go lang))

  (runner 'sub-section "Go language library")

  (runner 'test "len"
          (lambda () (eq? (len "") 0))
          (lambda () (eq? (len "abc") 3))
          (lambda () (eq? (len #vu8(1 2 3)) 3))
          (lambda () (eq? (len '(1 2 3)) 3))
          (lambda () (eq? (len '#(1 2 3)) 3))
          )
  )

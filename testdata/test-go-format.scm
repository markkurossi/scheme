;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(library (main)
  (export)
  (import (go format))

  (runner 'sub-section "Go format library")

  (runner 'test "format"
          (lambda () (eq? (format "") ""))
          (lambda () (eq? (format "a") "a"))
          (lambda () (eq? (format "a%%") "a%"))
          (lambda () (eq? (format "a%%b") "a%b"))
          (lambda () (eq? (format "%%a%%b") "%a%b"))
          )
  (runner 'test "format %d"
          (lambda () (eq? (format "%d" 42) "42"))
          (lambda () (eq? (format "%da" 42) "42a"))
          (lambda () (eq? (format "a%d" 42) "a42"))
          (lambda () (eq? (format "a%db" 42) "a42b"))
          )
  )

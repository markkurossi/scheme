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

  (runner 'test "format %"
          (lambda () (eq? (format "") ""))
          (lambda () (eq? (format "a") "a"))
          (lambda () (eq? (format "a%%") "a%"))
          (lambda () (eq? (format "a%%b") "a%b"))
          (lambda () (eq? (format "%%a%%b") "%a%b"))
          )
  (runner 'test "format %t"
          (lambda () (eq? (format "%t" #t) "true"))
          (lambda () (eq? (format "%t" #f) "false"))
          )
  (runner 'test "format %b"
          (lambda () (eq? (format "%b" 42) "101010"))
          (lambda () (eq? (format "%ba" 42) "101010a"))
          (lambda () (eq? (format "a%b" 42) "a101010"))
          (lambda () (eq? (format "a%bb" 42) "a101010b"))
          )
  (runner 'test "format %c"
          (lambda () (eq? (format "%c" 42) "*"))
          (lambda () (eq? (format "%c" 65) "A"))
          )
  (runner 'test "format %d"
          (lambda () (eq? (format "%d" 42) "42"))
          (lambda () (eq? (format "%da" 42) "42a"))
          (lambda () (eq? (format "a%d" 42) "a42"))
          (lambda () (eq? (format "a%db" 42) "a42b"))
          )
  (runner 'test "format %o"
          (lambda () (eq? (format "%o" 8) "10"))
          (lambda () (eq? (format "%o" 42) "52"))
          )
  (runner 'test "format %O"
          (lambda () (eq? (format "%O" 8) "0o10"))
          (lambda () (eq? (format "%O" 42) "0o52"))
          )
  (runner 'test "format %q"
          (lambda () (eq? (format "%q" 42) "#\\*"))
          (lambda () (eq? (format "%q" "f\\o\"o") "\"f\\\\o\\\"o\""))
          )
  (runner 'test "format %x"
          (lambda () (eq? (format "%x" 65535) "ffff"))
          )
  (runner 'test "format %X"
          (lambda () (eq? (format "%X" 65535) "FFFF"))
          )
  )

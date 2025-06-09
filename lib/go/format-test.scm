;;;
;;; Copyright (c) 2023-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(import (go format))

(define (test-format-% t)
  (t 'eq? (go::format "") "")
  (t 'eq? (go::format "a") "a")
  (t 'eq? (go::format "a%%") "a%")
  (t 'eq? (go::format "a%%b") "a%b")
  (t 'eq? (go::format "%%a%%b") "%a%b")
  )

(define (test-format-%b t)
  (t 'eq? (go::format "%b" 42) "101010")
  (t 'eq? (go::format "%ba" 42) "101010a")
  (t 'eq? (go::format "a%b" 42) "a101010")
  (t 'eq? (go::format "a%bb" 42) "a101010b")
  )

(define (test-format-%c t)
  (t 'eq? (go::format "%c" 42) "*")
  (t 'eq? (go::format "%c" 65) "A")
  )

(define (test-format-%d t)
  (t 'eq? (go::format "%d" 42) "42")
  (t 'eq? (go::format "%da" 42) "42a")
  (t 'eq? (go::format "a%d" 42) "a42")
  (t 'eq? (go::format "a%db" 42) "a42b")
  )

(define (test-format-%o t)
  (t 'eq? (go::format "%o" 8) "10")
  (t 'eq? (go::format "%o" 42) "52")
  )

(define (test-format-%O t)
  (t 'eq? (go::format "%O" 8) "0o10")
  (t 'eq? (go::format "%O" 42) "0o52")
  )

(define (test-format-%q t)
  (t 'eq? (go::format "%q" 42) "#\\*")
  (t 'eq? (go::format "%q" "f\\o\"o") "\"f\\\\o\\\"o\"")
  )

(define (test-format-%t t)
  (t 'eq? (go::format "%t" #t) "true")
  (t 'eq? (go::format "%t" #f) "false")
  )

(define (test-format-%v t)
  (t 'eq? (go::format "%v" #t) "true")
  (t 'eq? (go::format "%v" #f) "false")
  )

(define (test-format-%x t)
  (t 'eq? (go::format "%x" 65535) "ffff")
  )

(define (test-format-%X t)
  (t 'eq? (go::format "%X" 65535) "FFFF")
  )

(define (test-format-%-?0-9+d t)
  (t 'eq? (go::format "%10d" 42) "        42")
  (t 'eq? (go::format "%-10d" 42) "42        ")
  (t 'eq? (go::format "%010d" 42) "0000000042")
  (t 'eq? (go::format "%-010d" 42) "4200000000")
  )

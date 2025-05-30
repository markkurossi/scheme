;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (srfi 13))

(define (test-string-pad t)
  (t 'eq? (string-pad "foobar" 8 #\~) "~~foobar")
  (t 'eq? (string-pad "foobar" 3 #\~) "bar")
  (t 'eq? (string-pad-right "foobar" 8 #\~) "foobar~~")
  (t 'eq? (string-pad-right "foobar" 3 #\~) "foo")
  )

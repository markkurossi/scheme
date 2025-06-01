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
  )

(define (test-string-pad-right t)
  (t 'eq? (string-pad-right "foobar" 8 #\~) "foobar~~")
  (t 'eq? (string-pad-right "foobar" 3 #\~) "foo")
  )

(define (test-string-join t)
  (t 'eq? (string-join '("a" "b" "c") "~") "a~b~c")
  (t 'eq? (string-join '("a") "~") "a")
  (t 'eq? (string-join '("") "~") "")
  (t 'eq? (string-join '("a" "b" "c") "~") "a~b~c" 'infix)

  (t 'eq? (string-join '("a" "b" "c") "~" 'prefix) "~a~b~c")
  (t 'eq? (string-join '("a") "~" 'prefix) "~a")
  (t 'eq? (string-join '("") "~" 'prefix) "~")

  (t 'eq? (string-join '("a" "b" "c") "~" 'suffix) "a~b~c~")
  (t 'eq? (string-join '("a") "~" 'suffix) "a~")
  (t 'eq? (string-join '("") "~" 'suffix) "~")
  )

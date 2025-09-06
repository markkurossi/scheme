;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (srfi 98))

(define (test-get-environment-variable t)
  (t 'eq? (get-environment-variable "_________not__here____") #f)
  )

(define (test-get-environment-variables t)
  (t 'eq? (let ((success #t))
            (for-each (lambda (item)
                        (unless (string? (get-environment-variable (car item)))
                          (set! success #f)))
                      (get-environment-variables))
            success)
     #t)
  )

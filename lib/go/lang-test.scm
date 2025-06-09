;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (go lang))

(define (test-len t)
  (t 'eq? (len "") 0)
  (t 'eq? (len "abc") 3)
  (t 'eq? (len #vu8(1 2 3)) 3)
  (t 'eq? (len '(1 2 3)) 3)
  (t 'eq? (len '#(1 2 3)) 3)
  )

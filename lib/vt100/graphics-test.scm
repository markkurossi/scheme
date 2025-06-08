;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (vt100 graphics))

(define (test-hblock t)
  (t 'eq? (hblock 1 0.125) "\x258f;")
  (t 'eq? (hblock 1 0.250) "\x258e;")
  (t 'eq? (hblock 1 0.375) "\x258d;")
  (t 'eq? (hblock 1 0.500) "\x258c;")
  (t 'eq? (hblock 1 0.625) "\x258b;")
  (t 'eq? (hblock 1 0.750) "\x258a;")
  (t 'eq? (hblock 1 0.875) "\x2589;")
  (t 'eq? (hblock 1 1.000) "\x2588;")
  )

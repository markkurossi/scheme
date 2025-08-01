;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(library (main)
  (export)
  (import (rnrs control))

  (runner 'sub-section "5. Control structures")

  (runner 'test "unless"
          (lambda () (eq? (unless (> 3 2) 'less) #f))
          (lambda () (eq? (unless (< 3 2) 'less) 'less))
          )
  )

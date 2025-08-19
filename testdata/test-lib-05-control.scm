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

  (runner 'test "when"
          (lambda () (eq? (when (> 3 2) 'greater) 'greater))
          (lambda () (eq? (when (< 3 2) 'greater) #f))
          )
  (runner 'test "unless"
          (lambda () (eq? (unless (> 3 2) 'less) #f))
          (lambda () (eq? (unless (< 3 2) 'less) 'less))
          )
  (runner 'test "do"
          (lambda () (equal? (do ((vec (make-vector 5))
                                  (i 0 (+ i 1)))
                                 ((= i 5) vec)
                               (vector-set! vec i i))
                             '#(0 1 2 3 4)))
          (lambda () (equal? (let ((x '(1 3 5 7 9)))
                               (do ((x x (cdr x))
                                    (sum 0 (+ sum (car x))))
                                   ((null? x) sum)))
                             25))
          )
  )

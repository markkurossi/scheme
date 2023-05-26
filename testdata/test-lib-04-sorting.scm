;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(library (main)
  (export)
  (import (rnrs sorting))

  (runner 'sub-section "4. Sorting")

  (runner 'test "list-sort"
          (lambda () (equal? (list-sort < '(3 5 2 1)) '(1 2 3 5)))
          (lambda () (equal? (list-sort < '()) '()))
          (lambda () (equal? (list-sort < '(3)) '(3)))
          (lambda () (equal? (list-sort < '(1 2 3 5)) '(1 2 3 5)))

          ;; Test that sorting is stable.
          (lambda () (equal? (list-sort
                              (lambda (a b) (< (car a) (car b)))
                              '((3 . 1) (3 . 2) (3 . 3)
                                (2 . 1) (2 . 2) (2 . 3)))
                             '((2 . 1) (2 . 2) (2 . 3)
                               (3 . 1) (3 . 2) (3 . 3))))
          )
  (runner 'test "vector-sort"
          (lambda () (equal? (vector-sort < '#(3 5 2 1)) '#(1 2 3 5)))
          (lambda () (equal? (vector-sort < '#()) '#()))
          (lambda () (equal? (vector-sort < '#(3)) '#(3)))
          (lambda () (equal? (vector-sort < '#(1 2 3 5)) '#(1 2 3 5)))
          )
  )

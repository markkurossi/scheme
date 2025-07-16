;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.1. Base types")

(let ((values (list #t 'a #\a '#(1 2) '() '(a b) 42 "foo" null?))
      (match (lambda (pred values)
               (apply + (map (lambda (v)
                               (if (pred v) 1 0))
                             values)))))
  (runner 'test "predicates"
          (lambda () (eq? (match boolean? values) 1))
          (lambda () (eq? (match symbol? values) 1))
          (lambda () (eq? (match char? values) 1))
          (lambda () (eq? (match vector? values) 1))
          (lambda () (eq? (match null? values) 1))
          (lambda () (eq? (match pair? values) 1))
          (lambda () (eq? (match number? values) 1))
          (lambda () (eq? (match string? values) 1))
          (lambda () (eq? (match procedure? values) 1))
          )
  )

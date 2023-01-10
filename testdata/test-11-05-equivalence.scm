;;;
;;; Copyright (c) 2022-2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.5. Equivalence predicates")

(runner 'run "eq?"
        (lambda (t)
          (if (eq? #t #f)
              (t 'error "1: expected failure")))
        (lambda (t)
          (if (not (eq? #t #t))
              (t 'error "2: expected success")))
        (lambda (t)
          (if (not (eq? #f #f))
              (t 'error "3: expected success")))
        (lambda (t)
          (if (eq? 1 2)
              (t 'error "4: expected neq")))
        (lambda (t)
          (if (not (eq? 1 1))
              (t 'error "5: expected eq"))))

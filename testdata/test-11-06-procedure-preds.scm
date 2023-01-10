;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.6. Procedure predicates")

(runner 'test "procedure?"
        (lambda () (procedure? car))
        (lambda () (not (procedure? 'car)))
        (lambda () (procedure? (lambda (x) (* x x))))
        (lambda () (not (procedure? '(lambda (x) (* x x)))))
        )

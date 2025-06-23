;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.16. Iteration")

(runner 'test "named let"
        (lambda ()
          (equal? (let loop ((numbers '(3 -2 1 6 -5))
                             (nonneg '())
                             (neg '()))
                    (cond ((null? numbers) (list nonneg neg))
                          ((>= (car numbers) 0)
                           (loop (cdr numbers)
                                 (cons (car numbers) nonneg)
                                 neg))
                          ((< (car numbers) 0)
                           (loop (cdr numbers)
                                 nonneg
                                 (cons (car numbers) neg)))))
                  '((6 1 3) (-5 -2))
                  )
          )
        )

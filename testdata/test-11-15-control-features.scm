;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.15. Control features")

(runner 'test "apply"
        (lambda () (eq? (apply + (list 3 4)) 7))

        ;; XXX pow should be sqrt giving result 30
        (lambda () (eq? (letrec ((pow
                                  (lambda (x) (* x x)))
                                 (compose
                                  (lambda (f g)
                                    (lambda args
                                      (f (apply g args))))))
                          ((compose pow *) 12 75))
                        810000))
        )

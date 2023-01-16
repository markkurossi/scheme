;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.15. Control features")

(runner 'test "apply"
        (lambda () (eq? (apply + (list 3 4)) 7))
        (lambda () (eq? (letrec ((compose
                                  (lambda (f g)
                                    (lambda args
                                      (f (apply g args))))))
                          ((compose sqrt *) 12 75))
                        30))
        )

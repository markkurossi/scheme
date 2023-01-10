;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.8. Booleans")

(runner 'test "not"
        (lambda () (not (not #t)))
        (lambda () (not (not 3)))
        (lambda () (not (not (list 3))))
        (lambda () (not #f))
        (lambda () (not (not '())))
        (lambda () (not (not (list))))
        (lambda () (not (not 'nil)))
        )

(runner 'test "boolean?"
        (lambda () (boolean? #f))
        (lambda () (not (boolean? 0)))
        (lambda () (not (boolean? '())))
        )

(runner 'test "boolean=?"
        (lambda () (boolean=? #t #t))
        (lambda () (boolean=? #f #f))
        (lambda () (boolean=? #t #t #t))
        (lambda () (not (boolean=? #t #f)))
        (lambda () (not (boolean=? #t #t #f)))
        )

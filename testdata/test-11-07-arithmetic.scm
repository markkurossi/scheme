;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.7. Arithmetic")

(runner 'test "odd?"
        (lambda ()(odd? 1))
        (lambda ()(odd? #e1))
        )

(runner 'test "even?"
        (lambda () (even? 0))
        (lambda () (even? 42))
        (lambda () (even? #e42))
        )

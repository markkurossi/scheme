;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.7. Arithmetic")

(runner 'test "number?"
        (lambda () (number? 1))
        (lambda () (number? -1))
        (lambda () (number? #e42))
        )
(runner 'test "integer?"
        (lambda () (integer? 1))
        (lambda () (integer? -1))
        (lambda () (integer? #e42))
        )
(runner 'test "exact?"
        (lambda () (exact? #e1))
        (lambda () (not (exact? 1)))
        )
(runner 'test "inexact?"
        (lambda () (not (inexact? #e1)))
        (lambda () (inexact? 1))
        )

(runner 'test "="
        (lambda () (= 1 1))
        (lambda () (= 1 1 1))
        (lambda () (= #e-1 #e-1 #e-1))
        )
(runner 'test "<"
        (lambda () (< 4 5))
        (lambda () (not (< 4 3)))
        (lambda () (< #e4 #e5))
        (lambda () (not (< #e4 #e4)))
        (lambda () (not (< #e4 #e3)))
        (lambda () (< 4 #e5))
        (lambda () (not (< 4 #e4)))
        (lambda () (not (< 4 #e3)))
        (lambda () (< #e4 5))
        (lambda () (not (< #e4 4)))
        (lambda () (not (< #e4 3)))
        )
(runner 'test ">"
        (lambda () (not (> 4 5)))
        (lambda () (not (> 4 4)))
        (lambda () (> 4 3))
        (lambda () (not (> #e4 #e5)))
        (lambda () (not (> #e4 #e4)))
        (lambda () (> #e4 #e3))
        (lambda () (not (> 4 #e5)))
        (lambda () (not (> 4 #e4)))
        (lambda () (> 4 #e3))
        (lambda () (not (> #e4 5)))
        (lambda () (not (> #e4 4)))
        (lambda () (> #e4 3))
        )

(runner 'test "odd?"
        (lambda () (odd? 1))
        (lambda () (odd? #e1))
        )

(runner 'test "even?"
        (lambda () (even? 0))
        (lambda () (even? 42))
        (lambda () (even? #e42))
        )

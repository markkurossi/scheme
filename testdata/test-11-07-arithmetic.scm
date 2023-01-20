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
(runner 'test "<="
        (lambda () (<= 4 5))
        (lambda () (<= 4 4))
        (lambda () (not (<= 4 3)))
        (lambda () (<= #e4 #e5))
        (lambda () (<= #e4 #e4))
        (lambda () (not (<= #e4 #e3)))
        (lambda () (<= 4 #e5))
        (lambda () (<= 4 #e4))
        (lambda () (not (<= 4 #e3)))
        (lambda () (<= #e4 5))
        (lambda () (<= #e4 4))
        (lambda () (not (<= #e4 3)))
        )
(runner 'test ">="
        (lambda () (not (>= 4 5)))
        (lambda () (>= 4 4))
        (lambda () (>= 4 3))
        (lambda () (not (>= #e4 #e5)))
        (lambda () (>= #e4 #e4))
        (lambda () (>= #e4 #e3))
        (lambda () (not (>= 4 #e5)))
        (lambda () (>= 4 #e4))
        (lambda () (>= 4 #e3))
        (lambda () (not (>= #e4 5)))
        (lambda () (>= #e4 4))
        (lambda () (>= #e4 3))
        )

(runner 'test "zero?"
        (lambda () (zero? 0))
        (lambda () (zero? #e0))
        )
(runner 'test "positive?"
        (lambda () (positive? 1))
        (lambda () (positive? #e1))
        (lambda () (not (positive? 0)))
        (lambda () (not (positive? -1)))
        )
(runner 'test "negative?"
        (lambda () (negative? -1))
        (lambda () (negative? #e-1))
        (lambda () (not (negative? 0)))
        (lambda () (not (negative? 1)))
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

(runner 'test "max"
        (lambda () (= (max 3) 3))
        (lambda () (= (max 3 4) 4))
        (lambda () (= (max 3 5 4) 5))
        (lambda () (= (max 3 #e5 4 #e4) #e5))
        )
(runner 'test "min"
        (lambda () (= (min 3) 3))
        (lambda () (= (min 4 3) 3))
        (lambda () (= (min 4 3 5) 3))
        (lambda () (= (min 5 #e3 #e5 4) #e3))
        )

(runner 'test "+"
        (lambda () (= (+ 3 4) 7))
        (lambda () (= (+ 3) 3))
        (lambda () (= (+) 0))
        (lambda () (= (+ 1 2 3) 6))
        )
(runner 'test "*"
        (lambda () (= (* 4) 4))
        (lambda () (= (*) 1))
        (lambda () (= (* 1 2 3) 6))
        )
(runner 'test "-"
        (lambda () (= (- 3 4) -1))
        (lambda () (= (- 3 4 5) -6))
        (lambda () (= (- 3) -3))
        )
(runner 'test "/"
        (lambda () (= (/ 3) 0))
        (lambda () (= (/ 4 2) 2))
        (lambda () (= (/ #e4 2) #e2))
        (lambda () (= (/ 4 #e2) #e2))
        (lambda () (= (/ #e4 #e2) #e2))
        )

(runner 'test "mod"
        (lambda () (= (mod 5 2) 1))
        (lambda () (= (mod 4 2) 0))
        (lambda () (= (mod #e5 2) #e1))
        (lambda () (= (mod 5 #e2) #e1))
        (lambda () (= (mod #e5 #e2) #e1))
        )
(runner 'test "sqrt"
        (lambda () (= (sqrt 9) 3))
        (lambda () (= (sqrt #e9) #e3))
        )
(runner 'test "expt"
        (lambda () (= (expt 5 3) 125))
        (lambda () (= (expt #e5 3) #e125))
        (lambda () (= (expt 5 #e3) #e125))
        (lambda () (= (expt #e5 #e3) #e125))
        (lambda () (= (expt 5 0) 1))
        (lambda () (= (expt 0 0) 1))
        )

(letrec ((fact
          (lambda (n)
            (if (< n #e2)
                #e1
                (* n (fact (- n #e1)))))))
  (runner 'test "fact"
          (lambda () (eq? (fact 5) 120))
          (lambda () (eq? (fact #e5) #e120))
          ))

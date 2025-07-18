;;;
;;; Copyright (c) 2023, 2025 Markku Rossi
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
(runner 'test "float?"
        (lambda () (float? 1.0))
        (lambda () (float? -1.0))
        (lambda () (float? #e42.0))
        (lambda () (not (float? 1)))
        (lambda () (not (float? -1)))
        (lambda () (not (float? #e42)))
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
        (lambda () (< 4 5.0))
        (lambda () (< 4 #e5))
        (lambda () (< 4 #e5.0))

        (lambda () (< 4.0 5))
        (lambda () (< 4.0 5.0))
        (lambda () (< 4.0 #e5))
        (lambda () (< 4.0 #e5.0))

        (lambda () (< #e4 5))
        (lambda () (< #e4 5.0))
        (lambda () (< #e4 #e5))
        (lambda () (< #e4 #e5.0))

        (lambda () (< #e4.0 5))
        (lambda () (< #e4.0 5.0))
        (lambda () (< #e4.0 #e5))
        (lambda () (< #e4.0 #e5.0))

        (lambda () (not (< 4 3)))
        (lambda () (not (< 4 3.0)))
        (lambda () (not (< 4 #e3)))
        (lambda () (not (< 4 #e3.0)))

        (lambda () (not (< 4.0 3)))
        (lambda () (not (< 4.0 3.0)))
        (lambda () (not (< 4.0 #e3)))
        (lambda () (not (< 4.0 #e3.0)))

        (lambda () (not (< #e4 #e4)))
        (lambda () (not (< #e4 #e3)))
        (lambda () (not (< #e4 4)))
        (lambda () (not (< #e4 3)))

        (lambda () (not (< #e4.0 #e4)))
        (lambda () (not (< #e4.0 #e3)))
        (lambda () (not (< #e4.0 4)))
        (lambda () (not (< #e4.0 3)))
        )
(runner 'test ">"
        (lambda () (> 4 3))
        (lambda () (> 4 3.0))
        (lambda () (> 4 #e3))
        (lambda () (> 4 #e3.0))

        (lambda () (> 4.0 3))
        (lambda () (> 4.0 3.0))
        (lambda () (> 4.0 #e3))
        (lambda () (> 4.0 #e3.0))

        (lambda () (> #e4 3))
        (lambda () (> #e4 3.0))
        (lambda () (> #e4 #e3))
        (lambda () (> #e4 #e3.0))

        (lambda () (> #e4.0 3))
        (lambda () (> #e4.0 3.0))
        (lambda () (> #e4.0 #e3))
        (lambda () (> #e4.0 #e3.0))

        (lambda () (not (> 4 5)))
        (lambda () (not (> 4 5.0)))
        (lambda () (not (> 4 #e5)))
        (lambda () (not (> 4 #e5.0)))
        (lambda () (not (> 4 4)))

        (lambda () (not (> 4.0 5)))
        (lambda () (not (> 4.0 5.0)))
        (lambda () (not (> 4.0 #e5)))
        (lambda () (not (> 4.0 #e5.0)))

        (lambda () (not (> #e4 5)))
        (lambda () (not (> #e4 5.0)))
        (lambda () (not (> #e4 #e5)))
        (lambda () (not (> #e4 #e5.0)))

        (lambda () (not (> #e4.0 5)))
        (lambda () (not (> #e4.0 5.0)))
        (lambda () (not (> #e4.0 #e5)))
        (lambda () (not (> #e4.0 #e5.0)))
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
        (lambda () (eq? (+ 3) 3))
        (lambda () (eq? (+) 0))
        (lambda () (eq? (+ 1 2 3) 6))

        ;; Inlined binary.

        (lambda () (eq? (+ 3 4) 7))
        (lambda () (eq? (+ 3 4.0) 7.0))
        (lambda () (eq? (+ 3 #e4) #e7))
        (lambda () (eq? (+ 3 #e4.0) #e7.0))

        (lambda () (eq? (+ 3.0 4) 7.0))
        (lambda () (eq? (+ 3.0 4.0) 7.0))
        (lambda () (eq? (+ 3.0 #e4) #e7.0))
        (lambda () (eq? (+ 3.0 #e4.0) #e7.0))

        (lambda () (eq? (+ #e3 4) #e7))
        (lambda () (eq? (+ #e3 4.0) #e7.0))
        (lambda () (eq? (+ #e3 #e4) #e7))
        (lambda () (eq? (+ #e3 #e4.0) #e7.0))

        (lambda () (eq? (+ #e3.0 4) #e7.0))
        (lambda () (eq? (+ #e3.0 4.0) #e7.0))
        (lambda () (eq? (+ #e3.0 #e4) #e7.0))
        (lambda () (eq? (+ #e3.0 #e4.0) #e7.0))

        ;; Non-inlined.

        (lambda () (eq? (+ 3 4 1) 8))
        (lambda () (eq? (+ 3 4.0 1) 8.0))
        (lambda () (eq? (+ 3 #e4 1) #e8))
        (lambda () (eq? (+ 3 #e4.0 1) #e8.0))

        (lambda () (eq? (+ 3.0 4 1) 8.0))
        (lambda () (eq? (+ 3.0 4.0 1) 8.0))
        (lambda () (eq? (+ 3.0 #e4 1) #e8.0))
        (lambda () (eq? (+ 3.0 #e4.0 1) #e8.0))

        (lambda () (eq? (+ #e3 4 1) #e8))
        (lambda () (eq? (+ #e3 4.0 1) #e8.0))
        (lambda () (eq? (+ #e3 #e4 1) #e8))
        (lambda () (eq? (+ #e3 #e4.0 1) #e8.0))

        (lambda () (eq? (+ #e3.0 4 1) #e8.0))
        (lambda () (eq? (+ #e3.0 4.0 1) #e8.0))
        (lambda () (eq? (+ #e3.0 #e4 1) #e8.0))
        (lambda () (eq? (+ #e3.0 #e4.0 1) #e8.0))

        ;; Apply binary.

        (lambda () (eq? (apply + '(3 4)) 7))
        (lambda () (eq? (apply + '(3 4.0)) 7.0))
        (lambda () (eq? (apply + '(3 #e4)) #e7))
        (lambda () (eq? (apply + '(3 #e4.0)) #e7.0))

        (lambda () (eq? (apply + '(3.0 4)) 7.0))
        (lambda () (eq? (apply + '(3.0 4.0)) 7.0))
        (lambda () (eq? (apply + '(3.0 #e4)) #e7.0))
        (lambda () (eq? (apply + '(3.0 #e4.0)) #e7.0))

        (lambda () (eq? (apply + '(#e3 4)) #e7))
        (lambda () (eq? (apply + '(#e3 4.0)) #e7.0))
        (lambda () (eq? (apply + '(#e3 #e4)) #e7))
        (lambda () (eq? (apply + '(#e3 #e4.0)) #e7.0))

        (lambda () (eq? (apply + '(#e3.0 4)) #e7.0))
        (lambda () (eq? (apply + '(#e3.0 4.0)) #e7.0))
        (lambda () (eq? (apply + '(#e3.0 #e4)) #e7.0))
        (lambda () (eq? (apply + '(#e3.0 #e4.0)) #e7.0))

        ;; Result type checks.

        (lambda () (inexact? (+ 1 2)))
        (lambda () (inexact? (+ 1 2.0)))
        (lambda () (exact? (+ 1 #e2)))
        (lambda () (exact? (+ 1 #e2.0)))
        (lambda () (exact? (+ #e1 2)))
        (lambda () (exact? (+ #e1 2.0)))
        (lambda () (exact? (+ #e1 #e2)))
        (lambda () (exact? (+ #e1 #e2.0)))
        (lambda () (exact? (+ #e1.0 #e2)))
        (lambda () (exact? (+ #e1.0 #e2.0)))
        )
(runner 'test "*"
        (lambda () (eq? (* 4) 4))
        (lambda () (eq? (*) 1))
        (lambda () (eq? (* 1 2 3) 6))

        ;; Inlined binary

        (lambda () (eq? (* 2 3) 6))
        (lambda () (eq? (* 2 3.0) 6.0))
        (lambda () (eq? (* 2 #e3) #e6))
        (lambda () (eq? (* 2 #e3.0) #e6.0))

        (lambda () (eq? (* 2.0 3) 6.0))
        (lambda () (eq? (* 2.0 3.0) 6.0))
        (lambda () (eq? (* 2.0 #e3) #e6.0))
        (lambda () (eq? (* 2.0 #e3.0) #e6.0))

        (lambda () (eq? (* #e2 3) #e6))
        (lambda () (eq? (* #e2 3.0) #e6.0))
        (lambda () (eq? (* #e2 #e3) #e6))
        (lambda () (eq? (* #e2 #e3.0) #e6.0))

        (lambda () (eq? (* #e2.0 3) #e6.0))
        (lambda () (eq? (* #e2.0 3.0) #e6.0))
        (lambda () (eq? (* #e2.0 #e3) #e6.0))
        (lambda () (eq? (* #e2.0 #e3.0) #e6.0))

        ;; Non-inlined.

        (lambda () (eq? (* 2 3 1) 6))
        (lambda () (eq? (* 2 3.0 1) 6.0))
        (lambda () (eq? (* 2 #e3 1) #e6))
        (lambda () (eq? (* 2 #e3.0 1) #e6.0))

        (lambda () (eq? (* 2.0 3 1) 6.0))
        (lambda () (eq? (* 2.0 3.0 1) 6.0))
        (lambda () (eq? (* 2.0 #e3 1) #e6.0))
        (lambda () (eq? (* 2.0 #e3.0 1) #e6.0))

        (lambda () (eq? (* #e2 3 1) #e6))
        (lambda () (eq? (* #e2 3.0 1) #e6.0))
        (lambda () (eq? (* #e2 #e3 1) #e6))
        (lambda () (eq? (* #e2 #e3.0 1) #e6.0))

        (lambda () (eq? (* #e2.0 3 1) #e6.0))
        (lambda () (eq? (* #e2.0 3.0 1) #e6.0))
        (lambda () (eq? (* #e2.0 #e3 1) #e6.0))
        (lambda () (eq? (* #e2.0 #e3.0 1) #e6.0))

        ;; Apply binary

        (lambda () (eq? (apply * '(2 3)) 6))
        (lambda () (eq? (apply * '(2 3.0)) 6.0))
        (lambda () (eq? (apply * '(2 #e3)) #e6))
        (lambda () (eq? (apply * '(2 #e3.0)) #e6.0))

        (lambda () (eq? (apply * '(2.0 3)) 6.0))
        (lambda () (eq? (apply * '(2.0 3.0)) 6.0))
        (lambda () (eq? (apply * '(2.0 #e3)) #e6.0))
        (lambda () (eq? (apply * '(2.0 #e3.0)) #e6.0))

        (lambda () (eq? (apply * '(#e2 3)) #e6))
        (lambda () (eq? (apply * '(#e2 3.0)) #e6.0))
        (lambda () (eq? (apply * '(#e2 #e3)) #e6))
        (lambda () (eq? (apply * '(#e2 #e3.0)) #e6.0))

        (lambda () (eq? (apply * '(#e2.0 3)) #e6.0))
        (lambda () (eq? (apply * '(#e2.0 3.0)) #e6.0))
        (lambda () (eq? (apply * '(#e2.0 #e3)) #e6.0))
        (lambda () (eq? (apply * '(#e2.0 #e3.0)) #e6.0))

        ;; Result type checks.

        (lambda () (inexact? (* 2 2)))
        (lambda () (exact? (* 2 #e2)))
        (lambda () (exact? (* #e2 2)))
        (lambda () (exact? (* #e2 #e2)))
        )
(runner 'test "-"
        (lambda () (eq? (- 3 4) -1))
        (lambda () (eq? (- 3 4 5) -6))
        (lambda () (eq? (- 3) -3))

        ;; Inlined binary

        (lambda () (eq? (- 3 4) -1))
        (lambda () (eq? (- 3 4.0) -1.0))
        (lambda () (eq? (- 3 #e4) #e-1))
        (lambda () (eq? (- 3 #e4.0) #e-1.0))

        (lambda () (eq? (- 3.0 4) -1.0))
        (lambda () (eq? (- 3.0 4.0) -1.0))
        (lambda () (eq? (- 3.0 #e4) #e-1.0))
        (lambda () (eq? (- 3.0 #e4.0) #e-1.0))

        (lambda () (eq? (- #e3 4) #e-1))
        (lambda () (eq? (- #e3 4.0) #e-1.0))
        (lambda () (eq? (- #e3 #e4) #e-1))
        (lambda () (eq? (- #e3 #e4.0) #e-1.0))

        (lambda () (eq? (- #e3.0 4) #e-1.0))
        (lambda () (eq? (- #e3.0 4.0) #e-1.0))
        (lambda () (eq? (- #e3.0 #e4) #e-1.0))
        (lambda () (eq? (- #e3.0 #e4.0) #e-1.0))

        ;; Non-inlined.

        (lambda () (eq? (- 3 4 1) -2))
        (lambda () (eq? (- 3 4.0 1) -2.0))
        (lambda () (eq? (- 3 #e4 1) #e-2))
        (lambda () (eq? (- 3 #e4.0 1) #e-2.0))

        (lambda () (eq? (- 3.0 4 1) -2.0))
        (lambda () (eq? (- 3.0 4.0 1) -2.0))
        (lambda () (eq? (- 3.0 #e4 1) #e-2.0))
        (lambda () (eq? (- 3.0 #e4.0 1) #e-2.0))

        (lambda () (eq? (- #e3 4 1) #e-2))
        (lambda () (eq? (- #e3 4.0 1) #e-2.0))
        (lambda () (eq? (- #e3 #e4 1) #e-2))
        (lambda () (eq? (- #e3 #e4.0 1) #e-2.0))

        (lambda () (eq? (- #e3.0 4 1) #e-2.0))
        (lambda () (eq? (- #e3.0 4.0 1) #e-2.0))
        (lambda () (eq? (- #e3.0 #e4 1) #e-2.0))
        (lambda () (eq? (- #e3.0 #e4.0 1) #e-2.0))

        ;; Apply binary

        (lambda () (eq? (apply - '(3 4)) -1))
        (lambda () (eq? (apply - '(3 4.0)) -1.0))
        (lambda () (eq? (apply - '(3 #e4)) #e-1))
        (lambda () (eq? (apply - '(3 #e4.0)) #e-1.0))

        (lambda () (eq? (apply - '(3.0 4)) -1.0))
        (lambda () (eq? (apply - '(3.0 4.0)) -1.0))
        (lambda () (eq? (apply - '(3.0 #e4)) #e-1.0))
        (lambda () (eq? (apply - '(3.0 #e4.0)) #e-1.0))

        (lambda () (eq? (apply - '(#e3 4)) #e-1))
        (lambda () (eq? (apply - '(#e3 4.0)) #e-1.0))
        (lambda () (eq? (apply - '(#e3 #e4)) #e-1))
        (lambda () (eq? (apply - '(#e3 #e4.0)) #e-1.0))

        (lambda () (eq? (apply - '(#e3.0 4)) #e-1.0))
        (lambda () (eq? (apply - '(#e3.0 4.0)) #e-1.0))
        (lambda () (eq? (apply - '(#e3.0 #e4)) #e-1.0))
        (lambda () (eq? (apply - '(#e3.0 #e4.0)) #e-1.0))

        ;; Result type checks.

        (lambda () (inexact? (- 4 2)))
        (lambda () (inexact? (- 4 2.0)))
        (lambda () (exact? (- 4 #e2)))
        (lambda () (exact? (- 4 #e2.0)))
        (lambda () (exact? (- #e4 2)))
        (lambda () (exact? (- #e4 2.0)))
        (lambda () (exact? (- #e4 #e2)))
        (lambda () (exact? (- #e4 #e2.0)))
        (lambda () (exact? (- #e1.0 #e2)))
        (lambda () (exact? (- #e1.0 #e2.0)))
        )

(let ((val 42))
  (runner 'test "-const"
          (lambda () (eq? (- 42 1) 41))
          (lambda () (eq? (- (+ 40 2) 1) 41))
          (lambda () (eq? (- (+ val 0) 1) 41))
          )
  )

(runner 'test "/"
        (lambda () (eq? (/ 3) 0))

        ;; Inlined binary.

        (lambda () (eq? (/ 4 2) 2))
        (lambda () (eq? (/ 4 2.0) 2.0))
        (lambda () (eq? (/ 4 #e2) #e2))
        (lambda () (eq? (/ 4 #e2.0) #e2.0))

        (lambda () (eq? (/ 4.0 2) 2.0))
        (lambda () (eq? (/ 4.0 2.0) 2.0))
        (lambda () (eq? (/ 4.0 #e2) #e2.0))
        (lambda () (eq? (/ 4.0 #e2.0) #e2.0))

        (lambda () (eq? (/ #e4 2) #e2))
        (lambda () (eq? (/ #e4 2.0) #e2.0))
        (lambda () (eq? (/ #e4 #e2) #e2))
        (lambda () (eq? (/ #e4 #e2.0) #e2.0))

        (lambda () (eq? (/ #e4.0 2) #e2.0))
        (lambda () (eq? (/ #e4.0 2.0) #e2.0))
        (lambda () (eq? (/ #e4.0 #e2) #e2.0))
        (lambda () (eq? (/ #e4.0 #e2.0) #e2.0))

        ;; Non-inlined.

        (lambda () (eq? (/ 4 2 1) 2))
        (lambda () (eq? (/ 4 2.0 1) 2.0))
        (lambda () (eq? (/ 4 #e2 1) #e2))
        (lambda () (eq? (/ 4 #e2.0 1) #e2.0))

        (lambda () (eq? (/ 4.0 2 1) 2.0))
        (lambda () (eq? (/ 4.0 2.0 1) 2.0))
        (lambda () (eq? (/ 4.0 #e2 1) #e2.0))
        (lambda () (eq? (/ 4.0 #e2.0 1) #e2.0))

        (lambda () (eq? (/ #e4 2 1) #e2))
        (lambda () (eq? (/ #e4 2.0 1) #e2.0))
        (lambda () (eq? (/ #e4 #e2 1) #e2))
        (lambda () (eq? (/ #e4 #e2.0 1) #e2.0))

        (lambda () (eq? (/ #e4.0 2 1) #e2.0))
        (lambda () (eq? (/ #e4.0 2.0 1) #e2.0))
        (lambda () (eq? (/ #e4.0 #e2 1) #e2.0))
        (lambda () (eq? (/ #e4.0 #e2.0 1) #e2.0))

        ;; Apply binary.

        (lambda () (eq? (apply / '(4 2)) 2))
        (lambda () (eq? (apply / '(4 2.0)) 2.0))
        (lambda () (eq? (apply / '(4 #e2)) #e2))
        (lambda () (eq? (apply / '(4 #e2.0)) #e2.0))

        (lambda () (eq? (apply / '(4.0 2)) 2.0))
        (lambda () (eq? (apply / '(4.0 2.0)) 2.0))
        (lambda () (eq? (apply / '(4.0 #e2)) #e2.0))
        (lambda () (eq? (apply / '(4.0 #e2.0)) #e2.0))

        (lambda () (eq? (apply / '(#e4 2)) #e2))
        (lambda () (eq? (apply / '(#e4 2.0)) #e2.0))
        (lambda () (eq? (apply / '(#e4 #e2)) #e2))
        (lambda () (eq? (apply / '(#e4 #e2.0)) #e2.0))

        (lambda () (eq? (apply / '(#e4.0 2)) #e2.0))
        (lambda () (eq? (apply / '(#e4.0 2.0)) #e2.0))
        (lambda () (eq? (apply / '(#e4.0 #e2)) #e2.0))
        (lambda () (eq? (apply / '(#e4.0 #e2.0)) #e2.0))

        (lambda () (eq? (abs -7) 7))

        (lambda () (eq? (remainder 13 4) 1))
        (lambda () (eq? (remainder -13 4) -1))
        (lambda () (eq? (remainder 13 -4) 1))
        (lambda () (eq? (remainder -13 -4) -1))

        ;; Result type checks.

        (lambda () (inexact? (/ 4 2)))
        (lambda () (inexact? (/ 4 2.0)))
        (lambda () (exact? (/ 4 #e2)))
        (lambda () (exact? (/ 4 #e2.0)))
        (lambda () (exact? (/ #e4 2)))
        (lambda () (exact? (/ #e4 2.0)))
        (lambda () (exact? (/ #e4 #e2)))
        (lambda () (exact? (/ #e4 #e2.0)))
        (lambda () (exact? (/ #e4.0 2)))
        (lambda () (exact? (/ #e4.0 2.0)))
        (lambda () (exact? (/ #e4.0 #e2)))
        (lambda () (exact? (/ #e4.0 #e2.0)))
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

(runner 'test "number->string"
        (lambda () (eq? (number->string 42) "42"))
        (lambda () (eq? (number->string 42 2) "101010"))
        (lambda () (eq? (number->string 42 8) "52"))
        (lambda () (eq? (number->string 42 10) "42"))
        (lambda () (eq? (number->string 42 16) "2a"))
        )
(runner 'test "string->number"
        (lambda () (eq? (string->number "100") 100))
        (lambda () (eq? (string->number "100" 16) 256))
        (lambda () (eq? (string->number "#t") #f))
        )

(runner 'test "number->float"
        (lambda () (eq? (number->float 1) 1.0))
        (lambda () (eq? (number->float #e1) #e1.0))
        )
(runner 'test "number->integer"
        (lambda () (eq? (number->integer 1.2) 1))
        (lambda () (eq? (number->integer #e1.2) #e1))
        )

(letrec ((fact
          (lambda (n)
            (if (< n #e2)
                1
                (* n (fact (- n 1)))))))
  (runner 'test "fact"
          (lambda () (eq? (fact 5) 120))
          (lambda () (eq? (fact #e5) #e120))
          ))

;;;
;;; Copyright (c) 2022-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.4. Expressions")

(runner 'test "quotation"
        (lambda () (equal? (quote a) 'a))
        (lambda () (equal? (quote #(a b c)) '#(a b c)))
        (lambda () (equal? (quote (+ 1 2)) '(+ 1 2)))

        (lambda () (equal? '"abc" "abc"))
        (lambda () (equal? '145932 145932))
        (lambda () (equal? 'a 'a))
        (lambda () (equal? '#(a b c) '#(a b c)))
        (lambda () (equal? '() '()))
        (lambda () (equal? '(+ 1 2) '(+ 1 2)))
        (lambda () (equal? '(quote a) ''a))
        (lambda () (equal? ''a ''a))
        )

(runner 'test "procedures"
        (lambda () (eq? ((lambda (x) (+ x x)) 4) 8))
        (lambda () (eq? ((lambda (x)
                           (define (p y)
                             (+ y 1))
                           (+ (p x) x))
                         5)
                        11))
        (lambda () (eq? (letrec ((reverse-subtract
                                  (lambda (x y) (- y x))))
                          (reverse-subtract 7 10))
                        3))
        (lambda () (eq? (letrec ((add4
                                  (let ((x 4))
                                    (lambda (y) (+ x y)))))
                          (add4 6))
                        10))
        (lambda () (equal? ((lambda x x) 3 4 5 6)
                           '(3 4 5 6)))
        (lambda () (equal? ((lambda (x y . z) z)
                            3 4 5 6)
                           '(5 6)))
        )

(runner 'test "if"
        (lambda () (eq? (if (> 3 2) 'yes 'no) 'yes))
        (lambda () (eq? (if (> 2 3) 'yes 'no) 'no))
        (lambda () (eq? (if (> 3 2)
                            (- 3 2)
                            (+ 3 2))
                        1))
        ;; Unspecified.
        (lambda () (eq? (if #f #f) #f))
        )

(runner 'test "set!"
        (lambda () (eq? (let ((x 2))
                          (+ x 1)
                          (set! x 4)
                          (+ x 1))
                        5))
        )

;; 11.4.5. Derived conditionals

(runner 'test "cond"
        (lambda () (eq? (cond ((> 3 2) 'greater)
                              ((< 3 2) 'less))
                        'greater))
        (lambda () (eq? (cond ((> 3 3) 'greater)
                              ((< 3 3) 'less)
                              (else 'equal))
                        'equal))
        (lambda () (eq? (cond ((> 3 2))
                              ((< 3 3) 'less)
                              (else 'equal))
                        #t))
        (lambda () (eq? (cond ('(1 2 3) => cadr)
                              (else #f))
                        2))
        )

(runner 'test "case"
        (lambda () (eq? (case (* 2 3)
                          ((2 3 5 7) 'prime)
                          ((1 4 6 8 9) 'composite))
                        'composite))
        (lambda () (eq? (case (car '(c d))
                          ((a) 'a)
                          ((b) 'b))
                        #f))
        (lambda () (eq? (case (car '(c d))
                          ((a e i o u) 'vowel)
                          ((w y) 'semivowel)
                          (else 'consonant))
                        'consonant))
        )

(runner 'test "and"
        (lambda () (eq? (and (= 2 2) (> 2 1)) #t))
        (lambda () (eq? (and (= 2 2) (< 2 1)) #f))
        (lambda ()
          (pragma (check-boolean-exprs #f))
          (equal? (and 1 2 'c '(f g)) '(f g)))
        (lambda () (eq? (and) #t))
        )

(runner 'test "or"
        (lambda () (eq? (or) #f))
        (lambda () (eq? (or (= 2 2) (> 2 1)) #t))
        (lambda () (eq? (or (= 2 2) (< 2 1)) #t))
        (lambda () (eq? (or #f #f #f) #f))
        (lambda ()
          (pragma (check-boolean-exprs #f))
          (equal? (or '(b c) (/ 3 0)) '(b c)))
        )

;; 11.4.6. Binding constructs

(runner 'test "let"
        (lambda () (eq? (let ((x 2) (y 3))
                          (* x y))
                        6))
        (lambda () (eq? (let ((x 2) (y 3))
                          (let ((x 7)
                                (z (+ x y)))
                            (* z x)))
                        35))
        (lambda () (eq? (let () 42) 42))
        )
(runner 'test "let*"
        (lambda () (eq? (let ((x 2) (y 3))
                          (let* ((x 7)
                                 (z (+ x y)))
                            (* z x)))
                        70))
        )
(runner 'test "letrec"
        (lambda () (eq? (letrec ((even?
                                  (lambda (n)
                                    (if (zero? n)
                                        #t
                                        (odd? (- n 1)))))
                                 (odd?
                                  (lambda (n)
                                    (if (zero? n)
                                        #f
                                        (even? (- n 1))))))
                          (even? 88))
                        #t))
        )

;; 11.4.7. Sequencing

(runner 'test "begin"
        (lambda() (eq? (begin 1 2 3 4) 4))
        )

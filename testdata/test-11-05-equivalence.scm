;;;
;;; Copyright (c) 2022-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.5. Equivalence predicates")

(runner 'test "eqv?"
        (lambda () (eqv? 'a 'a))
        (lambda () (not (eqv? 'a 'b)))
        (lambda () (eqv? 2 2))
        (lambda () (eqv? '() '()))
        (lambda () (eqv? 100000000 100000000))
        (lambda () (not (eqv? (cons 1 2) (cons 1 2))))
        (lambda () (not (eqv? (lambda () 1)
                              (lambda () 2))))
        (lambda () (not (eqv? #f 'nil)))

        ;; Unspecified eqv? reusult.
        (lambda () (let ((p (lambda (x) x)))
                     (eqv? p p)))
        (lambda () (eqv? "" ""))
        (lambda () (eqv? '#() '#()))
        (lambda () (not (eqv? (lambda (x) x)
                              (lambda (x) x))))
        (lambda () (not (eqv? '(a) '(a))))
        (lambda () (eqv? "a" "a"))

        ;; Specified eqv? result.
        (lambda () (let ((x '(a)))
                     (eqv? x x)))
        )

(runner 'test "eq?"
        (lambda () (eq? 'a 'a))
        (lambda () (not (eq? '(a) '(a))))
        (lambda () (not (eq? (list 'a) (list 'a))))
        (lambda () (eq? "" ""))
        (lambda () (eq? '() '()))
        (lambda () (eq? 2 2))
        (lambda () (eq? #\A #\A))
        (lambda () (eq? car car))
        (lambda () (let ((n (+ 2 3)))
                     (eq? n n)))
        (lambda () (let ((x '()))
                     (eq? x x)))
        (lambda () (let ((x '#()))
                     (eq? x x)))
        (lambda () (let ((x (lambda (x) x)))
                     (eq? x x)))

        ;; Additional tests.
        (lambda () (not (eq? #t #f)))
        (lambda () (eq? #t #t))
        (lambda () (eq? #f #f))
        (lambda () (not (eq? 1 2)))
        (lambda () (eq? 1 1))
        )

(runner 'test "equal?"
        (lambda () (equal? 'a 'a))
        (lambda () (equal? '(a) '(a)))
        (lambda () (equal? '(a (b) c)
                           '(a (b) c)))
        (lambda () (equal? "abc" "abc"))
        (lambda () (equal? 2 2))
        (lambda () (equal? (make-vector 5 'a)
                           (make-vector 5 'a)))
        (lambda () (equal? '#vu8(1 2 3 4 5)
                           (u8-list->bytevector
                            '(1 2 3 4 5))))
        (lambda () (equal? (let* ((x (list 'a))
                                  (y (list 'a))
                                  (z (list x y)))
                             (list (equal? z (list y x))
                                   (equal? z (list x x))))
                           '(#t #t)))
        )

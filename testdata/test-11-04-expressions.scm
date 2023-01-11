;;;
;;; Copyright (c) 2022-2023 Markku Rossi
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
        (lambda () (equal? '#(a b c) #(a b c)))
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
        (lambda () (eq? (begin
                          (define reverse-subtract
                            (lambda (x y) (- y x)))
                          (reverse-subtract 7 10))
                        3))
        (lambda () (eq? (begin
                          (define add4
                            (let ((x 4))
                              (lambda (y) (+ x y))))
                          (add4 6))
                        10))
        (lambda () (equal? ((lambda x x) 3 4 5 6)
                           '(3 4 5 6)))
        (lambda () (equal? ((lambda (x y . z) z)
                            3 4 5 6)
                           '(5 6)))
        )

;; XXX if
;; XXX set!

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

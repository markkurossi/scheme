;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.13. Vectors")

(runner 'test "vector?"
        (lambda () (vector? '#(0 (2 2 2 2) "Anna")))
        (lambda () (not (vector? 1)))
        )
(runner 'test "make-vector"
        (lambda () (equal? (make-vector 5 #t)
                           '#(#t #t #t #t #t)))
        )
(runner 'test "vector-length"
        (lambda () (eq? (vector-length '#()) 0))
        (lambda () (eq? (vector-length '#(1 2 3)) 3))
        )
(runner 'test "vector-ref"
        (lambda () (eq? (vector-ref '#(1 1 2 3 5 8 13 21) 5) 8))
        )
(runner 'test "vector-set"
        (lambda () (equal? (let ((vec (vector 0 '(2 2 2 2) "Anna")))
                             (vector-set! vec 1 '("Sue" "Sue"))
                             vec)
                           '#(0 ("Sue" "Sue") "Anna")))
        )
(runner 'test "vector->list"
        (lambda () (equal? (vector->list '#(dah dah didah))
                           '(dah dah didah)))
        )
(runner 'test "list->vector"
        (lambda () (equal? (list->vector '(dididit dah))
                           '#(dididit dah)))
        )
(runner 'test "vector-fill"
        (lambda () (equal? (vector-fill! (make-vector 5) #t)
                           '#(#t #t #t #t #t)))
        )

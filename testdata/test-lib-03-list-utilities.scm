;;;
;;; Copyright (c) 2023, 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(library (main)
  (export)
  (import (rnrs lists))

  (runner 'sub-section "3. List utilities")

  (let ((loop '(1 1 1 1 1)))
    (set-cdr! (cddddr loop) loop)

    (runner 'test "find"
            (lambda () (eq? (find even? '(3 1 4 1 5 9)) 4))
            (lambda () (eq? (find even? '(3 1 5 1 5 9)) #f))
            (lambda () (error? (guard (con (else con))
                                      (find even? loop))))
            )

    (runner 'test "filter"
            (lambda () (equal? (filter even? '(3 1 4 1 5 9 2 6))
                               '(4 2 6)))
            )

    (runner 'test "remp"
            (lambda () (equal? (remp even? '(3 1 4 1 5 9 2 6 5))
                               '(3 1 1 5 9 5)))
            (lambda () (error? (guard (con (else con))
                                      (remp even? loop))))
            )
    (runner 'test "remove"
            (lambda () (equal? (remove 1 '(3 1 4 1 5 9 2 6 5))
                               '(3 4 5 9 2 6 5)))
            )
    (runner 'test "remv"
            (lambda () (equal? (remv 1 '(3 1 4 1 5 9 2 6 5))
                               '(3 4 5 9 2 6 5)))
            )
    (runner 'test "remq"
            (lambda () (equal? (remq 'foo '(foo bar baz))
                               '(bar baz)))
            )

    (runner 'test "memp"
            (lambda () (equal? (memp even? '(3 1 4 1 5 9 2 6 5))
                               '(4 1 5 9 2 6 5)))
            (lambda () (error? (guard (con (else con))
                                      (memp even? loop))))
            ))

  (runner 'test "memq"
          (lambda () (equal? (memq 'a '(a b c)) '(a b c)))
          (lambda () (equal? (memq 'b '(a b c)) '(b c)))
          (lambda () (equal? (memq 'a '(b c d)) #f))
          (lambda () (equal? (memq (list 'a) '(b (c) c)) #f))
          (lambda () (equal? (memq 101 '(100 101 102)) '(101 102)))
          )

  (runner 'test "member"
          (lambda () (equal? (member (list 'a) '(b (a) c)) '((a) c)))
          )

  (runner 'test "memv"
          (lambda () (equal? (memv 101 '(100 101 102)) '(101 102)))
          )

  (let ((d '((3 a) (1 b) (4 c)))
        (loop '((1 a) (3 b) (5 c) (7 d) (9 e))))
    (set-cdr! (cddddr loop) loop)
    (runner 'test "assp"
            (lambda () (equal? (assp even? d) '(4 c)))
            (lambda () (equal? (assp odd? d) '(3 a)))
            (lambda () (error? (guard (con (else con))
                                      (assp even? loop))))
            ))

  (let ((e '((a 1) (b 2) (c 3))))
    (runner 'test "assq"
            (lambda () (equal? (assq 'a e) '(a 1)))
            (lambda () (equal? (assq 'b e) '(b 2)))
            (lambda () (equal? (assq 'd e) #f))
            (lambda () (equal? (assq (list 'a) '(((a)) ((b)) ((c)))) #f))
            (lambda () (equal? (assq 5 '((2 3) (5 7) (11 13))) '(5 7)))
            ))

  (runner 'test "assoc"
          (lambda () (equal? (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a))))
          )

  (runner 'test "assv"
          (lambda () (equal? (assv 5 '((2 3) (5 7) (11 13))) '(5 7)))
          )
  )

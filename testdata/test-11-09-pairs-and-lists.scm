;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.9. Pairs and lists")

(runner 'test "pair?"
        (lambda () (pair? '(a . b)))
        (lambda () (pair? '(a b c)))
        (lambda () (not (pair? '())))
        (lambda () (not (pair? '#(a b))))
        )

(runner 'test "cons"
        (lambda () (equal? (cons 'a '()) '(a)))
        (lambda () (equal? (cons '(a) '(b c d)) '((a) b c d)))
        (lambda () (equal? (cons "a" '(b c)) '("a" b c)))
        (lambda () (equal? (cons 'a 3) '(a . 3)))
        (lambda () (equal? (cons '(a b) 'c) '((a b) . c)))
        )

(runner 'test "car"
        (lambda () (equal? (car '(a b c)) 'a))
        (lambda () (equal? (car '((a) b c d)) '(a)))
        (lambda () (equal? (car '(1 . 2)) 1))
        )
(runner 'test "cdr"
        (lambda () (equal? (cdr '((a) b c d))
                           '(b c d)))
        (lambda () (equal? (cdr '(1 . 2))
                           2))
        )
(runner 'test "null?"
        (lambda () (null? '()))
        (lambda () (null? (list)))
        (lambda () (not (null? 0)))
        (lambda () (not (null? (list 1))))
        )

(runner 'test "list?"
        (lambda () (list? '(a b c)))
        (lambda () (list? '()))
        (lambda () (list? (list)))
        (lambda () (not (list? '(a . b))))
        (lambda () (let ((loop '(1 2 3 4 5)))
                     (set-cdr! (cddddr loop) loop)
                     (not (list? loop))))
        )

(runner 'test "list"
        (lambda () (equal? (list 'a (+ 3 4) 'c) '(a 7 c)))
        (lambda () (equal? (list) '()))
        )

(runner 'test "length"
        (lambda () (= (length '(a b c)) 3))
        (lambda () (= (length '(a (b) (c d e))) 3))
        (lambda () (= (length '()) 0))
        )

(runner 'test "append"
        (lambda () (equal? (append '(x) '(y)) '(x y)))
        (lambda () (equal? (append '(a) '(b c d)) '(a b c d)))
        (lambda () (equal? (append '(a (b)) '((c))) '(a (b) (c))))
        (lambda () (equal? (append '(a b) '(c . d)) '(a b c . d)))
        (lambda () (equal? (append '() 'a) 'a))
        )

(runner 'test "reverse"
        (lambda () (equal? (reverse '(a b c)) '(c b a)))
        (lambda () (equal? (reverse '(a (b c) d (e (f))))
                           '((e (f)) d (b c) a)))
        )

(runner 'test "list-tail"
        (lambda () (equal? (list-tail '(a b c d) 2) '(c d)))
        )
(runner 'test "list-ref"
        (lambda () (equal? (list-ref '(a b c d) 2) 'c))
        )

(runner 'test "map"
        (lambda () (equal? (map cadr '((a b) (d e) (g h))) '(b e h)))
        (lambda () (equal? (map (lambda (n) (expt n n))
                                '(1 2 3 4 5))
                           '(1 4 27 256 3125)))
        (lambda () (equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9)))
        (lambda () (equal? (let ((count 0))
                             (map (lambda (ignored)
                                    (set! count (+ count 1))
                                    count)
                                  '(a b)))
                           '(1 2)))
        )

(runner 'test "for-each"
        (lambda () (equal? (let ((v (make-vector 5)))
                             (for-each (lambda (i)
                                         (vector-set! v i (* i i)))
                                       '(0 1 2 3 4))
                             v)
                           '#(0 1 4 9 16)))
        (lambda () (for-each (lambda (x) x) '(1 2 3 4)))
        (lambda () (for-each even? '()))
        )

(let ((loop-odd '(1 2 3 4 5))
      (loop-even '(1 2 3 4)))
  (set-cdr! (cddddr loop-odd) loop-odd)
  (set-cdr! (cdddr loop-even) loop-even)
  (runner 'test "loop"
          (lambda () (eq? (list? loop-odd) #f))
          (lambda () (eq? (list? loop-even) #f))
          (lambda () (eq? (length loop-odd) #f))
          (lambda () (eq? (length loop-even) #f))
          (lambda () (eq? (append '() loop-odd '(1 2)) #f))
          (lambda () (eq? (append '() loop-even '(1 2)) #f))
          (lambda () (eq? (reverse loop-odd) #f))
          (lambda () (eq? (reverse loop-even) #f))

          ;; Index must be big enough to detect the loop.
          (lambda () (eq? (list-tail loop-odd 100) #f))
          (lambda () (eq? (list-tail loop-even 100) #f))
          (lambda () (eq? (list-ref loop-odd 100) #f))
          (lambda () (eq? (list-ref loop-even 100) #f))

          (lambda () (eq? (for-each (lambda (x) #t) loop-odd) #f))
          (lambda () (eq? (for-each (lambda (x) #t) loop-even) #f))
          (lambda () (eq? (map (lambda (x) x) loop-odd) #f))
          (lambda () (eq? (map (lambda (x) x) loop-even) #f))
          ))

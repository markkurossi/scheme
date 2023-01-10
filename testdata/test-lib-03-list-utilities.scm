;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(runner 'sub-section "3. List utilities")

(runner 'run "memp"
        (lambda (t)
          (if (not (equal? (memp even? '(3 1 4 1 5 9 2 6 5)) '(4 1 5 9 2 6 5)))
              (t 'error "memp"))))

(runner 'run "memq"
        (lambda (t)
          (if (not (equal? (memq 'a '(a b c)) '(a b c)))
              (t 'error "memq 'a")))
        (lambda (t)
          (if (not (equal? (memq 'b '(a b c)) '(b c)))
              (t 'error "memq 'a")))
        (lambda (t)
          (if (not (equal? (memq 'a '(b c d)) #f))
              (t 'error "memq 'a")))
        (lambda (t)
          (if (not (equal? (memq (list 'a) '(b (c) c)) #f))
              (t 'error "memq 'a")))
        (lambda (t)
          (if (not (equal? (memq 101 '(100 101 102)) '(101 102)))
              (t 'error "memq 101")))
        )

(runner 'run "member"
        (lambda (t)
          (if (not (equal? (member (list 'a) '(b (a) c)) '((a) c)))
              (t 'error "member (list 'a)"))))

(runner 'run "memv"
        (lambda (t)
          (if (not (equal? (memv 101 '(100 101 102)) '(101 102)))
              (t 'error "memv 101"))))

(let ((d '((3 a) (1 b) (4 c))))
  (runner 'run "assp"
          (lambda (t)
            (if (not (equal? (assp even? d) '(4 c)))
                (t 'error "assp even?")))
          (lambda (t)
            (if (not (equal? (assp odd? d) '(3 a)))
                (t 'error "assp odd?")))
          ))

(let ((e '((a 1) (b 2) (c 3))))
  (runner 'run "assq"
          (lambda (t)
            (if (not (equal? (assq 'a e) '(a 1)))
                (t 'error "assq 'a")))
          (lambda (t)
            (if (not (equal? (assq 'b e) '(b 2)))
                (t 'error "assq 'b")))
          (lambda (t)
            (if (not (equal? (assq 'd e) #f))
                (t 'error "assq 'd")))
          (lambda (t)
            (if (not (equal? (assq (list 'a) '(((a)) ((b)) ((c)))) #f))
                (t 'error "assq (list 'a)")))
          (lambda (t)
            (if (not (equal? (assq 5 '((2 3) (5 7) (11 13))) '(5 7)))
                (t 'error "assq 5")))
          )
  )

(runner 'run "assoc"
        (lambda (t)
          (if (not (equal? (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a))))
              (t 'error "assoc"))))

(runner 'run "assv"
        (lambda (t)
          (if (not (equal? (assv 5 '((2 3) (5 7) (11 13))) '(5 7)))
              (t 'error "assv 5"))))

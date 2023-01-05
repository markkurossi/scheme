;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

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

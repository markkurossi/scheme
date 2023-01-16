;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (= z1 z2 . rest)
  (if (not (scheme::= z1 z2))
      #f
      (letrec ((iter
                (lambda (z values)
                  (if (null? values)
                      #t
                      (if (not (scheme::= z (car values)))
                          #f
                          (iter z (cdr values)))))))
        (iter z1 rest))))

(define (< x1 x2 . rest)
  (if (not (scheme::< x1 x2))
      #f
      (letrec ((iter
                (lambda (last values)
                  (if (null? values)
                      #t
                      (let ((next (car values))
                            (rest (cdr values)))
                        (if (not (scheme::< last next))
                            #f
                            (iter next rest)))))))
        (iter x2 rest))))

(define (> x1 x2 . rest)
  (if (not (scheme::> x1 x2))
      #f
      (letrec ((iter
                (lambda (last values)
                  (if (null? values)
                      #t
                      (let ((next (car values))
                            (rest (cdr values)))
                        (if (not (scheme::> last next))
                            #f
                            (iter next rest)))))))
        (iter x2 rest))))

(define (<= x1 x2 . rest)
  (if (scheme::> x1 x2)
      #f
      (letrec ((iter
                (lambda (last values)
                  (if (null? values)
                      #t
                      (let ((next (car values))
                            (rest (cdr values)))
                        (if (scheme::> last next)
                            #f
                            (iter next rest)))))))
        (iter x2 rest))))

(define (>= x1 x2 . rest)
  (if (scheme::< x1 x2)
      #f
      (letrec ((iter
                (lambda (last values)
                  (if (null? values)
                      #t
                      (let ((next (car values))
                            (rest (cdr values)))
                        (if (scheme::< last next)
                            #f
                            (iter next rest)))))))
        (iter x2 rest))))

(define (zero? z)
  (= z 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (max x . args)
  (letrec ((iter
            (lambda (best args)
              (if (null? args)
                  best
                  (let ((next (car args))
                        (rest (cdr args)))
                  (if (> next best)
                      (iter next rest)
                      (iter best rest)))))))
    (iter x args)))

(define (min x . args)
  (letrec ((iter
            (lambda (best args)
              (if (null? args)
                  best
                  (let ((next (car args))
                        (rest (cdr args)))
                  (if (< next best)
                      (iter next rest)
                      (iter best rest)))))))
    (iter x args)))

(define (+ . rest)
  (letrec ((iter
            (lambda (sum rest)
              (if (null? rest)
                  sum
                  (iter (scheme::+ sum (car rest)) (cdr rest))))))
    (iter 0 rest)))

(define (* . rest)
  (letrec ((iter
            (lambda (product rest)
              (if (null? rest)
                  product
                  (iter (scheme::* product (car rest)) (cdr rest))))))
    (iter 1 rest)))

(define (- z . rest)
  (if (null? rest)
      (scheme::- 0 z)
      (letrec ((iter
                (lambda (diff rest)
                  (if (null? rest)
                      diff
                      (iter (scheme::- diff (car rest)) (cdr rest))))))
        (iter z rest))))

(define (/ z . rest)
  (if (null? rest)
      (scheme::/ 1 z)
      (letrec ((iter
                (lambda (quot rest)
                  (if (null? rest)
                      quot
                      (iter (scheme::/ quot (car rest)) (cdr rest))))))
        (iter z rest))))

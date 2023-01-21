;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (scheme::compare pred x1 x2 rest)
  (if (pred x1 x2)
      (letrec ((iter
                (lambda (last values)
                  (if (null? values)
                      #t
                      (let ((next (car values))
                            (rest (cdr values)))
                        (if (pred last next)
                            (iter next rest)
                            #f))))))
        (iter x2 rest))
      #f))

(define (= z1 z2 . rest)
  (scheme::compare (lambda (x y) (scheme::= x y)) z1 z2 rest))

(define (< x1 x2 . rest)
  (scheme::compare (lambda (x y) (scheme::< x y)) x1 x2 rest))

(define (> x1 x2 . rest)
  (scheme::compare (lambda (x y) (scheme::> x y)) x1 x2 rest))

(define (<= x1 x2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::> x y))) x1 x2 rest))

(define (>= x1 x2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::< x y))) x1 x2 rest))

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
  (if (null? rest)
      #e0
      (letrec ((iter
                (lambda (sum rest)
                  (if (null? rest)
                      sum
                      (iter (scheme::+ sum (car rest)) (cdr rest))))))
        (iter (car rest) (cdr rest)))))

(define (* . rest)
  (if (null? rest)
      #e1
      (letrec ((iter
                (lambda (product rest)
                  (if (null? rest)
                      product
                      (iter (scheme::* product (car rest)) (cdr rest))))))
        (iter (car rest) (cdr rest)))))

(define (- z . rest)
  (if (null? rest)
      (scheme::- #e0 z)
      (letrec ((iter
                (lambda (diff rest)
                  (if (null? rest)
                      diff
                      (iter (scheme::- diff (car rest)) (cdr rest))))))
        (iter z rest))))

(define (/ z . rest)
  (if (null? rest)
      (scheme::/ #e1 z)
      (letrec ((iter
                (lambda (quot rest)
                  (if (null? rest)
                      quot
                      (iter (scheme::/ quot (car rest)) (cdr rest))))))
        (iter z rest))))

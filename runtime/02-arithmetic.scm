;;;
;;; Copyright (c) 2023, 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (= z1 z2 . rest)
  (scheme::compare scheme::= z1 z2 rest))

(define (< x1 x2 . rest)
  (scheme::compare scheme::< x1 x2 rest))

(define (> x1 x2 . rest)
  (scheme::compare scheme::> x1 x2 rest))

(define (<= x1 x2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::> x y))) x1 x2 rest))

(define (>= x1 x2 . rest)
  (scheme::compare (lambda (x y) (not (scheme::< x y))) x1 x2 rest))

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

(define (* . rest)
  (if (null? rest)
      1
      (letrec ((iter
                (lambda (product rest)
                  (if (null? rest)
                      product
                      (iter (scheme::* product (car rest)) (cdr rest))))))
        (iter (car rest) (cdr rest)))))

(define (/ z . rest)
  (if (null? rest)
      (scheme::/ 1 z)
      (letrec ((iter
                (lambda (quot rest)
                  (if (null? rest)
                      quot
                      (iter (scheme::/ quot (car rest)) (cdr rest))))))
        (iter z rest))))

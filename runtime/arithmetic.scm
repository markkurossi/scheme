;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

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

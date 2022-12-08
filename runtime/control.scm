;;;
;;; Copyright (c) 2022 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (apply proc arg . args)
  (if (null? args)
      (scheme::apply proc arg)
      (scheme::apply proc (append (list arg) args))))

(define (scheme::list-heads lists)
  (letrec ((heads
            (lambda (result lists)
              (if (null? lists)
                  (reverse result)
                  (heads (cons (caar lists) result)
                         (cdr lists))))))
    (heads '() lists)))

(define (scheme::list-tails lists)
  (letrec ((tails
            (lambda (result lists)
              (if (null? lists)
                  (reverse result)
                  (tails (cons (cdar lists) result)
                         (cdr lists))))))
    (tails '() lists)))

(define (map f . lists)
  (letrec ((loop
            (lambda (result lists)
              (if (null? (car lists))
                  (reverse result)
                  (loop (cons (apply f (scheme::list-heads lists)) result)
                        (scheme::list-tails lists))))))
    (loop '() lists)))

(define (for-each f . lists)
  (letrec ((loop
            (lambda (lists)
              (if (null? (car lists))
                  #t
                  (begin
                    (apply f (scheme::list-heads lists))
                    (loop (scheme::list-tails lists)))))))
    (loop lists)))

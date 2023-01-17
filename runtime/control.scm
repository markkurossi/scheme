;;;
;;; Copyright (c) 2022-2023 Markku Rossi
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
  (letrec ((turtle '())
           (iter
            (lambda (even result lists)
              (cond
               ((null? (car lists)) (reverse result))
               ((or (not (pair? (car lists)))
                    (eq? turtle (car lists)))
                ;; XXX (error 'map "not a list" f lists)
                #f)
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle (car lists))
                        (set! turtle (cdr turtle))))
                (iter (not even)
                      (cons (apply f (scheme::list-heads lists)) result)
                      (scheme::list-tails lists)))))))
    (iter #t '() lists)))

(define (for-each f . lists)
  (letrec ((turtle '())
           (iter
            (lambda (even lists)
              (cond
               ((null? (car lists)) #t)
               ((or (not (pair? (car lists)))
                    (eq? turtle (car lists)))
                ;; XXX (error 'for-each "not a list" f lists)
                #f)
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle (car lists))
                        (set! turtle (cdr turtle))))
                (apply f (scheme::list-heads lists))
                (iter (not even) (scheme::list-tails lists)))))))
    (iter #t lists)))

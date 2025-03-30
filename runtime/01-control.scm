;;;
;;; Copyright (c) 2022-2024 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define-constant (apply proc arg . args)
  (if (null? args)
      (scheme::apply proc arg)
      (scheme::apply proc (append (list arg) args))))

(define (scheme::list-heads lists)
  (letrec ((heads
            (lambda (head tail lists)
              (if (null? lists)
                  head
                  (let ((p (cons (caar lists) '())))
                    (if (null? tail)
                        (set! head p)
                        (set-cdr! tail p))
                    (set! tail p)
                    (heads head tail (cdr lists)))))))
    (heads '() '() lists)))

(define (scheme::list-tails lists)
  (letrec ((tails
            (lambda (head tail lists)
              (if (null? lists)
                  head
                  (let ((p (cons (cdar lists) '())))
                    (if (null? tail)
                        (set! head p)
                        (set-cdr! tail p))
                    (set! tail p)
                    (tails head tail (cdr lists)))))))
    (tails '() '() lists)))

(define (map f . lists)
  (letrec ((turtle '())
           (iter
            (lambda (even head tail lists)
              (cond
               ((null? (car lists)) head)
               ((or (not (pair? (car lists)))
                    (eq? turtle (car lists)))
                ;; XXX (error 'map "not a list" f lists)
                #f)
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle (car lists))
                        (set! turtle (cdr turtle))))
                (let ((p (cons (apply f (scheme::list-heads lists)) '())))
                  (if (null? tail)
                      (set! head p)
                      (set-cdr! tail p))
                  (set! tail p)
                  (iter (not even)
                        head tail
                        (scheme::list-tails lists))))))))
    (iter #t '() '() lists)))

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

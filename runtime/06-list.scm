;;;
;;; Copyright (c) 2022-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (list? obj) (guard (con (else #f))
                           (for-each (lambda (x) #t) obj)))

(define (list . items) items)

(define (length list)
  (let ((count 0))
    (for-each (lambda (x) (set! count (+ count 1))) list)
    count))

(define (append list . rest)
  (if (null? rest)
      list
      (letrec ((head '())
               (tail '())
               (append-list
                (lambda (items)
                  (for-each (lambda (x)
                              (let ((p (cons x '())))
                                (if (null? tail)
                                    (set! head p)
                                    (set-cdr! tail p))
                                (set! tail p)))
                            items)))
               (iter
                (lambda (rest)
                  (if (null? (cdr rest))
                      (if (null? tail)
                          (car rest)
                          (begin
                            (set-cdr! tail (car rest))
                            head))
                      (begin
                        (append-list (car rest))
                        (iter (cdr rest)))))))
        (append-list list)
        (iter rest))))

(define (reverse list)
  (letrec ((turtle '())
           (iter
            (lambda (even result rest)
              (cond
               ((null? rest) result)
               ((or (not (pair? rest))
                    (eq? turtle rest))
                (error 'reverse "not a list" list))
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle rest)
                        (set! turtle (cdr turtle))))
                (iter (not even)
                      (cons (car rest) result)
                      (cdr rest)))))))
    (iter #t '() list)))

(define (list-tail list k)
  (letrec ((turtle '())
           (iter
            (lambda (even lst index)
              (cond
               ((zero? index) lst)
               ((null? lst)
                (error 'list-tail "index out of range" list k))
               ((or (not (pair? lst))
                    (eq? turtle lst))
                (error 'list-tail "not a list" list))
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle lst)
                        (set! turtle (cdr turtle))))
                (iter (not even) (cdr lst) (- index 1)))))))
    (iter #t list k)))

(define (list-ref list k)
  (letrec ((turtle '())
           (iter
            (lambda (even lst index)
              (cond
               ((null? lst)
                (error 'list-ref "index out of range" list k))
               ((or (not (pair? lst))
                    (eq? turtle lst))
                (error 'list-ref "not a list" list))
               ((zero? index) (car lst))
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle lst)
                        (set! turtle (cdr turtle))))
                (iter (not even) (cdr lst) (- index 1)))))))
    (iter #t list k)))

;;;
;;; Copyright (c) 2022-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (list? obj) (for-each (lambda (x) #t) obj))

(define (list . items) items)

(define (length lst)
  (let ((count 0))
    (if (for-each (lambda (x) (set! count (+ count 1))) lst)
        count
        ;; XXX (error 'length "not a list" lst)
        -1)))

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
                      (if (append-list (car rest))
                          (iter (cdr rest))
                          #f)))))
        (if (append-list list)
            (iter rest)
            ;; XXX (error 'append "not a list" list rest)
            #f))))

(define (reverse list)
  (letrec ((turtle '())
           (iter
            (lambda (even result rest)
              (cond
               ((null? rest) result)
               ((or (not (pair? rest))
                    (eq? turtle rest))
                ;; XXX (error 'reverse "not a list" list)
                #f)
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
            (lambda (even list k)
              (cond
               ((zero? k) list)
               ((null? list)
                ;; XXX (error 'list-tail "index out of range" list k)
                #f)
               ((or (not (pair? list))
                    (eq? turtle list))
                ;; XXX (error 'list-tail "not a list" list k)
                #f)
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle list)
                        (set! turtle (cdr turtle))))
                (iter (not even) (cdr list) (- k 1)))))))
    ;; XXX (assertion-violation 'list-tail "index out of range" list k)
    (iter #t list k)))

(define (list-ref list k)
  (letrec ((turtle '())
           (iter
            (lambda (even list k)
              (cond
               ((null? list)
                ;; XXX (error 'list-ref "index out of range" list k)
                #f)
               ((or (not (pair? list))
                    (eq? turtle list))
                ;; XXX (error 'list-ref "not a list" list k)
                #f)
               ((zero? k) (car list))
               (else
                (if even
                    (if (null? turtle)
                        (set! turtle list)
                        (set! turtle (cdr turtle))))
                (iter (not even) (cdr list) (- k 1)))))))
    ;; XXX (assertion-violation 'list-ref "index out of range" list k)
    (iter #t list k)))

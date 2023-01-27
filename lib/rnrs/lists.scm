;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (rnrs lists (6))
  (export find
          remp remove remv remq
          memp member memv memq
          assp assoc assv assq)
  (import (rnrs))

  (define (find proc list)
    (letrec ((turtle '())
             (iter
              (lambda (even proc list)
                (cond
                 ((null? list) #f)
                 ((or (not (pair? list))
                      (eq? turtle list))
                  ;; XXX (error 'find "not a list" proc list)
                  #f)
                 (else
                  (if even
                      (if (null? turtle)
                          (set! turtle list)
                          (set! turtle (cdr turtle))))
                  (if (proc (car list))
                      (car list)
                      (iter (not even) proc (cdr list))))))))
      (iter #t proc list)))

  ;; XXX  for-all
  ;; XXX  exists
  ;; XXX  filter
  ;; XXX  partition
  ;; XXX  fold-left
  ;; XXX  fold-right

  (define (remp proc list)
    (letrec ((head '())
             (tail '())
             (turtle '())
             (iter
              (lambda (even proc list)
                (cond
                 ((null? list) head)
                 ((or (not (pair? list))
                      (eq? turtle list))
                  ;; XXX (error 'remp "not a list" proc list)
                  #f)
                 (else
                  (if even
                      (if (null? turtle)
                          (set! turtle list)
                          (set! turtle (cdr turtle))))
                  (if (not (proc (car list)))
                      (let ((p (cons (car list) '())))
                        (if (null? tail)
                            (set! head p)
                            (set-cdr! tail p))
                        (set! tail p)))
                  (iter (not even) proc (cdr list)))))))
      (iter #t proc list)))

  (define (remove obj list)
    (remp (lambda (item) (equal? obj item)) list))

  (define (remv obj list)
    (remp (lambda (item) (eqv? obj item)) list))

  (define (remq obj list)
    (remp (lambda (item) (eq? obj item)) list))

  (define (memp proc list)
    (letrec ((turtle '())
             (iter
              (lambda (even proc list)
                (cond
                 ((null? list) #f)
                 ((or (not (pair? list))
                      (eq? turtle list))
                  ;; XXX (error 'memp "not a list" proc list)
                  #f)
                 (else
                  (if even
                      (if (null? turtle)
                          (set! turtle list)
                          (set! turtle (cdr turtle))))
                  (if (proc (car list))
                      list
                      (iter (not even) proc (cdr list))))))))
      (iter #t proc list)))

  (define (member obj list)
    (memp (lambda (item) (equal? obj item)) list))

  (define (memv obj list)
    (memp (lambda (item) (eqv? obj item)) list))

  (define (memq obj list)
    (memp (lambda (item) (eq? obj item)) list))

  (define (assp proc alist)
    (letrec ((turtle '())
             (iter
              (lambda (even proc alist)
                (cond
                 ((null? alist) #f)
                 ((or (not (pair? alist))
                      (eq? turtle alist))
                  ;; XXX (error 'assp "not a list" proc alist)
                  #f)
                 (else
                  (if even
                      (if (null? turtle)
                          (set! turtle alist)
                          (set! turtle (cdr turtle))))

                  (let ((item (car alist)))
                    (if (proc (car item))
                        item
                        (iter (not even) proc (cdr alist)))))))))
      (iter #t proc alist)))

  (define (assoc obj alist)
    (assp (lambda (item) (equal? obj item)) alist))

  (define (assv obj alist)
    (assp (lambda (item) (eqv? obj item)) alist))

  (define (assq obj alist)
    (assp (lambda (item) (eq? obj item)) alist))

  ;; XXX cons*
  )

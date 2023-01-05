;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

;; rnrs lists (6)

(define (assp proc alist)
  (if (null? alist)
      #f
      (let ((item (car alist)))
        (if (proc (car item))
            item
            (assp proc (cdr alist))))))

(define (assoc obj alist)
  (assp (lambda (item) (equal? obj item)) alist))

(define (assv obj alist)
  (assp (lambda (item) (eqv? obj item)) alist))

(define (assq obj alist)
  (assp (lambda (item) (eq? obj item)) alist))

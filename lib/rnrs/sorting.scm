;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (rnrs sorting (6))
  (export list-sort)
  (import (rnrs base))

  (define (list-sort proc lst)
    (if (or (null? lst) (<= (length lst) 1))
        lst
        (let ((pivot (car lst))
              (lt '())
              (lt-tail '())
              (ge '())
              (ge-tail '()))
          (for-each (lambda (item)
                      (let ((p (cons item '())))
                        (if (proc item pivot)
                            (begin
                              (if (null? lt-tail)
                                  (set! lt p)
                                  (set-cdr! lt-tail p))
                              (set! lt-tail p))
                            (begin
                              (if (null? ge-tail)
                                  (set! ge p)
                                  (set-cdr! ge-tail p))
                              (set! ge-tail p)))))
                    (cdr lst))
          (append (list-sort proc lt)
                  (list pivot)
                  (list-sort proc ge)))))
  )

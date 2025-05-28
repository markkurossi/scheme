;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; SRFI 13: String Libraries - https://srfi.schemers.org/srfi-13/srfi-13.html
;;;

(library (srfi 13 (2024 9 2))
  (export string-pad string-pad-right string-join)
  (import (rnrs base))

  (define (string-pad s len . args)
    (let ((l (string-length s))
          (ch #\space))
      (if (not (null? args))
          (set! ch (car args)))
      (if (>= l len)
          (substring s (- l len) l)
          (string-append (make-string (- len l) ch) s))))

  (define (string-pad-right s len . args)
    (let ((l (string-length s))
          (ch #\space))
      (if (not (null? args))
          (set! ch (car args)))
      (if (>= l len)
          (substring s 0 len)
          (string-append s (make-string (- len l) ch)))))

  (define (string-join strings separator)
    (letrec ((iter
              (lambda (strings result)
                (if (null? strings)
                    result
                    (if (zero? (string-length result))
                        (iter (cdr strings) (car strings))
                        (iter (cdr strings)
                              (string-append result separator
                                             (car strings))))))))
      (iter strings "")))
  )

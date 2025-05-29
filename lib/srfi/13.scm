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

  (define (string-join strings . args)
    (letrec ((iter-infix
              (lambda (strings separator result)
                (if (null? strings)
                    result
                    (if (zero? (string-length result))
                        (iter-infix (cdr strings) separator (car strings))
                        (iter-infix (cdr strings) separator
                                    (string-append result separator
                                                   (car strings)))))))
             (iter-suffix
              (lambda (strings separator result)
                (if (null? strings)
                    (string-append result separator)
                    (if (zero? (string-length result))
                        (iter-suffix (cdr strings) separator (car strings))
                        (iter-suffix (cdr strings) separator
                                     (string-append result separator
                                                    (car strings)))))))

             (iter-prefix
              (lambda (strings separator result)
                (if (null? strings)
                    (string-append separator result)
                    (if (zero? (string-length result))
                        (iter-prefix (cdr strings) separator (car strings))
                        (iter-prefix (cdr strings) separator
                                     (string-append result separator
                                                    (car strings)))))))
             )
      (case (length args)
        ((0) (iter-infix strings " " ""))
        ((1) (iter-infix strings (car args) ""))
        ((2) (case (cadr args)
               ((infix)  (iter-infix  strings (car args) ""))
               ((prefix) (iter-prefix strings (car args) ""))
               ((suffix) (iter-suffix strings (car args) ""))
               (else
                (error 'srfi13 "Invalid grammar" (cadr args)))))
        (else
         (error 'srf113 "Invalid arguments" args)))))
  )

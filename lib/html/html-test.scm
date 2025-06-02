;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (html unescape))

(define (unescape t idx)
  (if (>= idx (vector-length html-entities))
      #t
      (let* ((item (vector-ref html-entities idx))
             (trailer " ")
             (expected (string-append (cdr item) trailer))
             (result (html-unescape (string-append "&" (car item) trailer))))
        (if (string=? result expected)
            (unescape t (+ idx 1))
            (begin
              (t 'error (format "failed to decode ~a: expected '~a', got '~a'"
                                (car item) expected result))
              (unescape t (+ idx 1)))))))

(define (test-unescape t)
  (unescape t 0))

;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (html unescape (1 0))
  (export html-unescape)
  (import (rnrs base) (rnrs lists))

  (define entities
    '(("lt" "<")
      ("gt" ">")
      ("amp" "&")
      ))

  (define (html-unescape input)
    (letrec ((scan-string
              (lambda (result start pos)
                (cond
                 ((>= pos (string-length input))
                  (string-append result (substring input start pos)))
                 ((char=? (string-ref input pos) #\&)
                  (scan-tag (string-append result (substring input start pos))
                            (+ pos 1) (+ pos 1)))
                 (else
                  (scan-string result start (+ pos 1))))))
             (scan-tag
              (lambda (result start pos)
                (cond
                 ((>= pos (string-length input))
                  (string-append result "{unclosed tag}"))
                 ((char=? (string-ref input pos) #\;)
                  (scan-string (string-append
                                result
                                (tag->string (substring input start pos)))
                               (+ pos 1) (+ pos 1)))
                 (else
                  (scan-tag result start (+ pos 1))))))
             (tag->string
              (lambda (tag)
                (case (string-downcase tag)
                  (("lt") "<")
                  (("gt") ">")
                  (("amp") "&")
                  (else (string-append "{" tag "}")))))
             )
      (scan-string "" 0 0)))
  )

;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (html escape (1 0))
  (export html-escape)
  (import (rnrs base))

  (define (html-escape input)
    (apply string-append
           (map (lambda (item)
                  (if (char? item)
                      (string item)
                      item))
                (map (lambda (ch)
                       (case ch
                         ((#\<) "&lt;")
                         ((#\>) "&gt;")
                         ((#\&) "&amp;")
                         ((#\') "&#39;") ; "&#39;" is shorter than "&apos;".
                         ((#\") "&#34;") ; "&#34;" is shorter than "&quot;".
                         (else ch)))
                     (string->list input)))))

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

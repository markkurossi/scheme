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
  )

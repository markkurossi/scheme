;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (go lang (1 19))
  (export len)
  (import (rnrs))

  ;; (display "This is initializer for the (go lang) library.") (newline)

  (define (len obj)
    (cond
     ((string? obj) (string-length obj))
     ((bytevector? obj) (bytevector-length obj))
     ((list? obj) (length obj))
     ((vector? obj) (vector-length obj))
     (else
      (error 'len "invalid object" obj))))
  )

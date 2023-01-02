;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (load filename)
  (let ((stack (scheme::stack-trace)))
    ((cadddr (scheme::load (caadr stack) filename)))))

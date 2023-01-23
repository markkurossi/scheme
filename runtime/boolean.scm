;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (boolean=? bool1 bool2 . rest)
  (scheme::compare (lambda (x y) (scheme::boolean=? x y)) bool1 bool2 rest))

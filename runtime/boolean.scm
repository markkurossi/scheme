;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (boolean=? bool1 bool2 . rest)
  (scheme::compare scheme::boolean=? bool1 bool2 rest))

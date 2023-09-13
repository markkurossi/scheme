;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (vt100 cursor (1 0))
  (export cursor-move)
  (import (vt100 base) (rnrs base))

  (define (cursor-move row col)
    (vt100-csi) (display row) (display ";") (display col) (display "H"))

  (define (erase-line-head)
    (vt100-csi) (display "1K"))
  (define (erase-line-tail)
    (vt100-csi) (display "K"))
  (define (erase-line)
    (vt100-csi) (display "2K"))

  (define (erase-screen-head)
    (vt100-csi) (display "1J"))
  (define (erase-screen-tail)
    (vt100-csi) (display "J"))
  (define (erase-screen)
    (vt100-csi) (display "2J"))
  )

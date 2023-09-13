;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (vt100 text (1 0))
  (export text-reset text-bold text-dim text-italic text-underscore)
  (import (vt100 base) (rnrs base))

  (define (text spec)
    (vt100-csi) (display spec) (display "m"))

  (define (text-default)
    (text "0"))
  (define (text-bold)
    (text "1"))
  (define (text-dim)
    (text "2"))
  (define (text-italic)
    (text "3"))
  (define (text-underscore)
    (text "4"))

  (define (text-fg-black)
    (text "30"))
  (define (text-fg-red)
    (text "31"))
  (define (text-fg-green)
    (text "32"))
  (define (text-fg-yellow)
    (text "33"))
  (define (text-fg-blue)
    (text "34"))
  (define (text-fg-magenta)
    (text "35"))
  (define (text-fg-cyan)
    (text "36"))
  (define (text-fg-white)
    (text "37"))
  )

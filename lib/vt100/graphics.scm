;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2023-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (vt100 graphics (1 0))
  (export hblock)
  (import (vt100 base) (rnrs base))

  (define (hblock width fract)
    (if (< fract 0.0)
        (set! fract 0.0))
    (if (> fract 1.0)
        (set! fract 1.0))
    (letrec ((w8 (number->float (* width 8)))
             (w (number->integer (* w8 fract)))
             (wrem (mod w 8))
             (fill (lambda (str count ch)
                     (if (zero? count)
                         str
                         (fill (string-append str ch) (- count 1) ch))))
             (result ""))
      (set! result (fill result (/ w 8) "\x2588;"))
      (if (> wrem 0)
          (set! result
                (string-append result
                               (list->string
                                (list (integer->char (- #x2590 wrem)))))))
      (fill result (- width (string-length result)) " ")))
  )

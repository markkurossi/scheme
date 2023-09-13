;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (vt100 base (1 0))
  (export vt100-csi)
  (import (rnrs base))

  (define (vt100-csi)
    (display "\x1b;["))
  )

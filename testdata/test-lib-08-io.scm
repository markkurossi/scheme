;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "8. I/O")

(runner 'test "eof-object"
        (lambda () (eqv? (eof-object) (eof-object)))
        (lambda () (eq? (eof-object) (eof-object)))
        )

(runner 'test "eof-object?"
        (lambda () (eq? (eof-object? (eof-object)) #t))
        (lambda () (eq? (eof-object? 42) #f))
        )

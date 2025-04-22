;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (html unescape (1 0))
  (export html-unescape)
  (import (rnrs base) (html entities))

  ;;(pragma (verbose-typecheck #t))

  (define (html-unescape input)
    (letrec ((input-length (string-length input))

             (scan-string
              (lambda (result start pos)
                (cond
                 ((>= pos input-length)
                  (string-append result (substring input start pos)))
                 ((char=? (string-ref input pos) #\&)
                  (scan-tag (string-append result (substring input start pos))
                            (+ pos 1) (+ pos 1)))
                 (else
                  (scan-string result start (+ pos 1))))))

             (scan-tag
              (lambda (result start pos)
                (if (>= pos input-length)
                    (string-append result (tag (substring input start pos)))
                    (let ((ch (string-ref input pos)))
                      (cond
                       ((or (and (char<=? #\a ch) (char<=? ch #\z))
                            (and (char<=? #\A ch) (char<=? ch #\Z))
                            (and (char<=? #\0 ch) (char<=? ch #\9)))
                        (scan-tag result start (+ pos 1)))
                       ((char=? ch #\;)
                        (scan-string (string-append
                                      result
                                      (tag (substring input start (+ pos 1))))
                                     (+ pos 1) (+ pos 1)))
                       (else
                        (scan-string (string-append
                                      result
                                      (tag (substring input start pos)))
                                     pos pos)))))))

             (tag
              (lambda (name)
                (let ((entity (lookup-entity
                               name 0 (vector-length html-entities))))
                  (if entity
                      entity
                      (string-append "{" name "}")))))

             (lookup-entity
              (lambda (name from to)
                (if (>= from to)
                    #f
                    (let* ((mid (/ (+ from to) 2))
                           (e (vector-ref html-entities mid))
                           (n (car e)))
                      (cond
                       ((string=? name n) (cdr e))
                       ((string<? name n) (lookup-entity name from mid))
                       (else (lookup-entity name (+ mid 1) to)))))))
             )
      (scan-string "" 0 0)))
  )

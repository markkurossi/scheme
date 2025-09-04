;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "7. Exceptions and conditions")

(runner 'test "with-exception-handler"
        (lambda ()
          (eq? (with-exception-handler
                (lambda (con)
                  "handler")
                (lambda ()
                  42))
               42))
        (lambda ()
          (eq? (with-exception-handler
                (lambda (con)
                  "handler")
                (lambda ()
                  (error 'handler "error")))
               "handler"))
        ;; Test that tail call works in handler.
        (lambda ()
          (letrec ((loop
                    (lambda (n)
                      (with-exception-handler
                       (lambda (con)
                         (if (zero? n)
                             #t
                             (loop (- n 1))))
                       (lambda ()
                         (error 'handler "thunk"))))))
            (loop (* (scheme::stack-size) 2))))
        ;; Test that tail call works in thunk.
        (lambda ()
          (letrec ((loop
                    (lambda (n)
                      (with-exception-handler
                       (lambda (con)
                         #f)
                       (lambda ()
                         (if (zero? n)
                             #t
                             (loop (- n 1))))))))
            (loop (* (scheme::stack-size) 2))))
        )

(runner 'test "guard"
        (lambda ()
          (eq? (guard (con
                       (else "handler"))
                      42)
               42))
        (lambda ()
          (eq? (guard (con
                       (else "handler"))
                      (error 'handler "error"))
               "handler"))
        ;; Test that tail call works in handler.
        (lambda ()
          (letrec ((loop
                    (lambda (n)
                      (guard (con
                              (else
                               (if (zero? n)
                                   #t
                                   (loop (- n 1)))))
                             (error 'handler "thunk")))))
            (loop (* (scheme::stack-size) 2))))
        ;; Test that tail call works in thunk.
        (lambda ()
          (letrec ((loop
                    (lambda (n)
                      (guard (con
                              (else
                               #f))
                             (if (zero? n)
                                 #t
                                 (loop (- n 1)))))))
            (loop (* (scheme::stack-size) 2))))
        )

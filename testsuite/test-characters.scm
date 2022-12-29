;;;
;;; Copyright (c) 2022 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "characters"
        (lambda (t)
          (letrec ((iter
                    (lambda (values)
                      (if (null? values)
                          #t
                          (begin
                            (if (not (eq? (caar values) (cadar values)))
                                (t 'error "character mismatch")
                                (iter (cdr values))))))))
            (iter '((#\a #\x61)
                    (#\A #\x41)
                    (#\( #\x28)
                    (#\  #\x20)
                    )))))

;;;
;;; Copyright (c) 2022 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "characters"
        (lambda (t)
          (letrec ((iter
                    (lambda (names values)
                      (if (null? names)
                          #t
                          (begin
                            (if (not (eq? (car names) (car values)))
                                (t 'error "character mismatch")
                                (iter (cdr names) (cdr values))))))))
            (iter '(#\a #\A)
                  '(#\x61 #\x41)))))

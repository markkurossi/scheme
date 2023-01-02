;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "strings"
        (lambda (t)
          (letrec ((iter
                    (lambda (values)
                      (if (null? values)
                          #t
                          (begin
                            (if (not (equal? (string->list (caar values))
                                             (cadar values)))
                                (t 'error "string mismatch"))
                            (iter (cdr values)))))))
            (iter '(("abc" (#\x61 #\x62 #\x63))
                    ("\x41;bc" (#\x41 #\x62 #\x63))
                    ("\x41; bc" (#\x41 #\x20 #\x62 #\x63))
                    ("\x41bc;" (#\x41bc))
                    ("\x00000041;" (#\x41))
                    ("\x0010FFFF;" (#\x10FFFF))
                    ("\x000000001;" (#\x1))
                    )))))

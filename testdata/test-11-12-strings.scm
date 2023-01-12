;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.12. Strings")

(runner 'test "strings"
        (lambda ()
          (letrec ((errors 0)
                   (iter
                    (lambda (values)
                      (if (null? values)
                          #t
                          (begin
                            (if (not (equal? (string->list (caar values))
                                             (cadar values)))
                                (set! errors (+ errors 1)))
                            (iter (cdr values)))))))
            (iter '(("abc" (#\x61 #\x62 #\x63))
                    ("\x41;bc" (#\x41 #\x62 #\x63))
                    ("\x41; bc" (#\x41 #\x20 #\x62 #\x63))
                    ("\x41bc;" (#\x41bc))
                    ("\x00000041;" (#\x41))
                    ("\x0010FFFF;" (#\x10FFFF))
                    ("\x000000001;" (#\x1))
                    ))
            (= errors 0)
            )))

(runner 'test "string?"
        (lambda () (string? ""))
        (lambda () (not (string? #\a)))
        )
(runner 'test "make-string"
        (lambda () (string=? (make-string 5) "     "))
        (lambda () (string=? (make-string 5 #\@) "@@@@@"))
        (lambda () (string=? (make-string 2 #\x41bc) "\x41bc;\x41bc;"))
        )

(runner 'test "string"
        (lambda () (string=? (string #\a #\b #\  #\c) "ab c"))
        )

(runner 'test "string-length"
        (lambda () (= (string-length "") 0))
        (lambda () (= (string-length "foo") 3))
        (lambda () (= (string-length "\x41bc;") 1))
        )

(runner 'test "string-ref"
        (lambda () (char=? (string-ref ">\x41bc;<" 0) #\>))
        (lambda () (char=? (string-ref ">\x41bc;<" 1) #\x41bc))
        (lambda () (char=? (string-ref ">\x41bc;<" 2) #\<))
        )

(runner 'test "string=?"
        (lambda () (string=? "" ""))
        (lambda () (string=? "" "" ""))
        (lambda () (string=? "abc" "abc" "abc"))
        (lambda () (not (string=? "abc" "abc" "abcd")))
        )

(runner 'test "string<?"
        (lambda () (string<? "a" "b"))
        (lambda () (string<? "a" "b" "c"))
        (lambda () (not (string<? "a" "b" "b")))
        )

(runner 'test "string>?"
        (lambda () (string>? "b" "a"))
        (lambda () (string>? "c" "b" "a"))
        (lambda () (not (string>? "c" "b" "b")))
        )

(runner 'test "string<=?"
        (lambda () (string<=? "a" "b"))
        (lambda () (string<=? "a" "b" "c"))
        (lambda () (string<=? "a" "b" "b"))
        (lambda () (not (string<=? "a" "b" "a")))
        )

(runner 'test "string=>?"
        (lambda () (string>=? "b" "a"))
        (lambda () (string>=? "c" "b" "a"))
        (lambda () (string>=? "c" "b" "b"))
        (lambda () (not (string>=? "c" "b" "c")))
        )

(runner 'test "substring"
        (lambda () (string=? (substring "foo" 0 0) ""))
        (lambda () (string=? (substring "foo" 0 1) "f"))
        (lambda () (string=? (substring "foo" 1 2) "o"))
        (lambda () (not (not (string=? (substring "foo" 1 3) "oo"))))
        )

(runner 'test "string-append"
        (lambda () (string=? (string-append) ""))
        (lambda () (string=? (string-append ">" "\x41bc;" "<") ">\x41bc;<"))
        )

(runner 'test "string->list"
        (lambda () (equal? (string->list "a\x41bc;c") '(#\a #\x41bc #\c)))
        (lambda () (equal? (string->list "") '()))
        )

(runner 'test "list->string"
        (lambda () (string=? (list->string (string->list "a\x41bc;c"))
                             "a\x41bc;c"))
        (lambda () (string=? (list->string (string->list "")) ""))
        )

(runner 'test "string-copy"
        (lambda () (string=? (string-copy "") ""))
        (lambda () (string=? (string-copy "a\x41bc;c") "a\x41bc;c"))
        )

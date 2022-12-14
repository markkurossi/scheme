;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.12. Strings")

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

(runner 'run "string?"
        (lambda (t)
          (if (not (string? ""))
              (t 'error "char? \"\"")))
        (lambda (t)
          (if (string? #\a)
              (t 'error "char? #\\a"))))
(runner 'run "make-string"
        (lambda (t)
          (if (not (string=? (make-string 5) "     "))
              (t 'error "make-string 5")))
        (lambda (t)
          (if (not (string=? (make-string 5 #\@) "@@@@@"))
              (t 'error "make-string @")))
        (lambda (t)
          (if (not (string=? (make-string 2 #\x41bc) "\x41bc;\x41bc;"))
              (t 'error "make-string u41bc"))))
(runner 'run "string"
        (lambda (t)
          (if (not (string=? (string #\a #\b #\  #\c) "ab c"))
              (t 'error "string"))))
(runner 'run "string-length"
        (lambda (t)
          (if (not (= (string-length "") 0))
              (t 'error "string-length 0")))
        (lambda (t)
          (if (not (= (string-length "foo") 3))
              (t 'error "string-length 3")))
        (lambda (t)
          (if (not (= (string-length "\x41bc;") 1))
              (t 'error "string-length \\x41bc"))))
(runner 'run "string-ref"
        (lambda (t)
          (if (not (char=? (string-ref ">\x41bc;<" 0) #\>))
              (t 'error "string-ref 1")))
        (lambda (t)
          (if (not (char=? (string-ref ">\x41bc;<" 1) #\x41bc))
              (t 'error "string-ref 1")))
        (lambda (t)
          (if (not (char=? (string-ref ">\x41bc;<" 2) #\<))
              (t 'error "string-ref 1"))))

(runner 'run "string=?"
        (lambda (t)
          (if (not (string=? "" ""))
              (t 'error "string=? empty")))
        (lambda (t)
          (if (not (string=? "" "" ""))
              (t 'error "string=? empty")))
        (lambda (t)
          (if (not (string=? "abc" "abc" "abc"))
              (t 'error "string=? abc")))
        (lambda (t)
          (if (string=? "abc" "abc" "abcd")
              (t 'error "string=? abc abcd"))))

(runner 'run "string<?"
        (lambda (t)
          (if (not (string<? "a" "b"))
              (t 'error "string<? a b")))
        (lambda (t)
          (if (not (string<? "a" "b" "c"))
              (t 'error "string<? a b c")))
        (lambda (t)
          (if (string<? "a" "b" "b")
              (t 'error "string<? a b b"))))
(runner 'run "string>?"
        (lambda (t)
          (if (not (string>? "b" "a"))
              (t 'error "string>? b a")))
        (lambda (t)
          (if (not (string>? "c" "b" "a"))
              (t 'error "string>? c b a")))
        (lambda (t)
          (if (string>? "c" "b" "b")
              (t 'error "string>? c b b"))))
(runner 'run "string<=?"
        (lambda (t)
          (if (not (string<=? "a" "b"))
              (t 'error "string<=? a b")))
        (lambda (t)
          (if (not (string<=? "a" "b" "c"))
              (t 'error "string<=? a b c")))
        (lambda (t)
          (if (not (string<=? "a" "b" "b"))
              (t 'error "string<=? a b b")))
        (lambda (t)
          (if (string<=? "a" "b" "a")
              (t 'error "string<=? a b a"))))
(runner 'run "string=>?"
        (lambda (t)
          (if (not (string>=? "b" "a"))
              (t 'error "string>=? b a")))
        (lambda (t)
          (if (not (string>=? "c" "b" "a"))
              (t 'error "string>=? c b a")))
        (lambda (t)
          (if (not (string>=? "c" "b" "b"))
              (t 'error "string>=? c b b")))
        (lambda (t)
          (if (string>=? "c" "b" "c")
              (t 'error "string>=? c b c"))))

(runner 'run "substring"
        (lambda (t)
          (if (not (string=? (substring "foo" 0 0) ""))
              (t 'error "substring 0 0")))
        (lambda (t)
          (if (not (string=? (substring "foo" 0 1) "f"))
              (t 'error "substring 0 1")))
        (lambda (t)
          (if (not (string=? (substring "foo" 1 2) "o"))
              (t 'error "substring 1 2")))
        (lambda (t)
          (if (not (string=? (substring "foo" 1 3) "oo"))
              (t 'error "substring 1 3"))))

(runner 'run "string-append"
        (lambda (t)
          (if (not (string=? (string-append) ""))
              (t 'error "string-append")))
        (lambda (t)
          (if (not (string=? (string-append ">" "\x41bc;" "<") ">\x41bc;<"))
              (t 'error "string-append"))))

(runner 'run "string->list"
        (lambda (t)
          (if (not (equal? (string->list "a\x41bc;c")
                           '(#\a #\x41bc #\c)))
              (t 'error "string->list")))
        (lambda (t)
          (if (not (equal? (string->list "") '()))
              (t 'error "string->append"))))
(runner 'run "list->string"
        (lambda (t)
          (if (not (string=? (list->string (string->list "a\x41bc;c"))
                             "a\x41bc;c"))
              (t 'error "list->string")))
        (lambda (t)
          (if (not (string=? (list->string (string->list "")) ""))
              (t 'error "string->append"))))

(runner 'run "string-copy"
        (lambda (t)
          (if (not (string=? (string-copy "") ""))
              (t 'error "string-copy")))
        (lambda (t)
          (if (not (string=? (string-copy "a\x41bc;c") "a\x41bc;c"))
              (t 'error "string-copy"))))

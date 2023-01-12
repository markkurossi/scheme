;;;
;;; Copyright (c) 2022-2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.11. Characters")

(runner 'test "characters"
        (lambda ()
          (letrec ((errors 0)
                   (iter
                    (lambda (values)
                      (if (null? values)
                          #t
                          (begin
                            (if (not (eq? (caar values) (cadar values)))
                                (set! errors (+ errors 1)))
                            (iter (cdr values)))))))
            (iter '((#\a 		#\x61)
                    (#\A 		#\x41)
                    (#\( 		#\x28)
                    (#\  		#\x20)
                    (#\nul 		#\x00)
                    (#\alarm		#\x07)
                    (#\backspace	#\x08)
                    (#\tab		#\x09)
                    (#\linefeed		#\x0a)
                    (#\newline		#\x0a)
                    (#\vtab		#\x0b)
                    (#\page		#\x0c)
                    (#\return		#\x0d)
                    (#\esc		#\x1b)
                    (#\space		#\x20)
                    (#\delete		#\x7f)
                    (#\xFF		#\xff)
                    (#\x03bb		#\x03bb)
                    (#\x00006587	#\x6587)
                    (#\xA		#\xa)
                    (#\xFF		#\xFF)
                    (#\xff		#\xFF)
                    ))
            (= errors 0)
            )))
(runner 'test "char?"
        (lambda () (char? #\a))
        (lambda () (not (char? 'a)))
        (lambda () (not (char? #t)))
        (lambda () (not (char? "foo")))
        (lambda () (not (char? 42)))
        (lambda () (not (char? cons)))
        )

(runner 'test "char->integer"
        (lambda () (= (char->integer #\return) #x0d))
        (lambda () (= (char->integer (integer->char 5000)) 5000))
        (lambda () (eq? (integer->char 32) #\space))
        )
(runner 'test "char predicates"
        (lambda () (char=? (integer->char 32) #\space #\ ))
        (lambda () (char=? #\space #\ ))
        (lambda () (char=? #\space #\ #\x20))
        (lambda () (not (char=? #\space #\x21)))

        (lambda () (char<? #\a #\b))
        (lambda () (char<? #\a #\b #\c))
        (lambda () (not (char<? #\a #\a)))

        (lambda () (char>? #\b #\a))
        (lambda () (char>? #\c #\b #\a))
        (lambda () (not (char>? #\a #\a)))

        (lambda () (char<=? #\a #\b))
        (lambda () (char<=? #\a #\b #\c))
        (lambda () (char<=? #\a #\b #\c #\c))
        (lambda () (not (char<=? #\b #\a)))

        (lambda () (char>=? #\b #\b))
        (lambda () (char>=? #\c #\b #\a))
        (lambda () (char>=? #\c #\b #\a #\a))
        (lambda () (not (char>=? #\a #\b)))
        )

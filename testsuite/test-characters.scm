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
                                (t 'error "character mismatch"))
                            (iter (cdr values)))))))
            (iter '((#\a 		#\x61)
                    (#\A 		#\x41)
                    (#\( 		#\x28)
                    (#\  		#\x20)
                    (#\nul 		#\x00)
                    (#\alarm		#\x007)
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
                    )))))

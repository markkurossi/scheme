;;;
;;; Copyright (c) 2022-2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (test name)
  (letrec ((count 0)
           (success 0)
           (fail 0)
           (run-test
            (lambda (category idx num-tests test)
              (let ((failed #f))
                ;; Run test with reporter.
                (test (lambda (cmd . args)
                        (cond ((eq? cmd 'error)
                               (set! failed #t))
                              (else
                               (display "Test reporter: invalid command: ")
                               (display cmd)
                               (newline)))))
                (if failed
                    (begin
                      (set! fail (+ fail 1))
                      (color "1;31")
                      (display #\x2716)
                      (color "0"))
                    (begin
                      (set! success (+ success 1))
                      (color "1;32")
                      (display #\x2713)
                      (color "0")))
                )))
           (iter
            (lambda (category num-tests tests)
              (if (null? tests)
                  #t
                  (begin
                    (set! count (+ count 1))
                    (run-test category
                              (- num-tests (length tests))
                              num-tests
                              (car tests))
                    (iter category num-tests (cdr tests))))))

           (test-success
            (lambda ()
              (set! success (+ success 1))
              (color "1;32")
              (display #\x2713)
              (color "0")))
           (test-failure
            (lambda ()
              (set! fail (+ fail 1))
              (color "1;31")
              (display #\x2716)
              (color "0")))

           (test-iter
            (lambda (tests)
              (if (null? tests)
                  #t
                  (begin
                    (set! count (+ count 1))
                    (if ((car tests))
                        (test-success)
                        (test-failure))
                    (test-iter (cdr tests))))))
           (runner
            (lambda (cmd . args)
              (case cmd
                ((run)
                 (display " - test ") (display (car args)) (display ": ")
                 (iter (car args) (length (cdr args)) (cdr args))
                 (newline))
                ((sub-section)
                 (display "** ") (display (car args)) (newline))
                ((test)
                 (display " - test ") (display (car args)) (display ": ")
                 (test-iter (cdr args))
                 (newline))
                ((error)
                 (set! fail (+ fail 1)))
                ((stats)
                 (display "Tests  : ") (display count) (newline)
                 (display "Success: ") (display success) (newline)
                 (display "Fail   : ") (display fail) (newline))
                ((status)
                 (= fail 0))
                (else
                 (display "Unknown command: ") (display cmd) (newline)
                 (display "Usage:") (newline)
                 (display " - run group tests...") (newline)
                 (display " - stats") (newline)
                 (exit 1))))))
    runner))

(define (color spec)
  (if (zero? (string-length (getenv "INSIDE_EMACS")))
      (begin
        (display "\x1b;[") (display spec) (display "m"))))

(define runner (test "r6rs"))

(load "test-eq.scm")
(load "test-cond.scm")
(load "test-case.scm")
(load "test-and.scm")
(load "test-or.scm")
(load "test-characters.scm")
(load "test-strings.scm")
(load "test-numbers.scm")
(load "test-lists.scm")
(load "test-symbols.scm")
(load "test-bytevectors.scm")

(load "test-r6rs-lists.scm")

(runner 'stats)
(runner 'status)

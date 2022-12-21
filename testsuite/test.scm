;;;
;;; Copyright (c) 2022 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (test name)
  (letrec ((count 0)
           (success 0)
           (fail 0)
           (run-test
            (lambda (category idx num-tests test)
              (display "running ") (display category) (display " tests: ")
              (display (+ idx 1)) (display " / ") (display num-tests) (newline)
              (let ((of fail))
                (test runner)
                (if (= of fail)
                    (set! success (+ success 1))))))
           (iter
            (lambda (category num-tests tests)
              (if (null? tests)
                  #t
                  (begin
                    (set! count (+ count 1))
                    ;;((car tests) runner)
                    (run-test category
                              (- num-tests (length tests))
                              num-tests
                              (car tests))
                    (iter category num-tests (cdr tests))))))
           (runner
            (lambda (cmd . args)
              (cond ((eq? cmd 'run)
                     (iter (car args) (length (cdr args)) (cdr args)))
                    ((eq? cmd 'error)
                     (set! fail (+ fail 1)))
                    ((eq? cmd 'stats)
                     (display "tests  : ") (display count) (newline)
                     (display "success: ") (display success) (newline)
                     (display "fail   : ") (display fail) (newline))
                    ((eq? cmd 'status)
                     (= fail 0))
                    (else
                     (display "Usage:") (newline)
                     (display " - run tests...") (newline)
                     (display " - stats") (newline))))))
    runner))

(define runner (test "r6rs"))

(load "test-eq.scm")

(runner 'stats)
(runner 'status)

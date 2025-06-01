;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (scheme test (1 0))
  (export scheme::test::runner)
  (import (rnrs base) (filepath walk) (srfi 13) (srfi 28))

  (define scheme::test::count 0)
  (define scheme::test::errors 0)
  (define scheme::test::log '())

  (define (scheme::test::runner verbose . args)
    (if (null? args)
        (scheme::test::handle-arg ".")
        (for-each scheme::test::handle-arg args)))


  (define (scheme::test::handle-arg arg)
    (if (string-suffix? "/..." arg)
        (scheme::test::traverse (substring arg 0 (- (string-length arg) 4)))
        (scheme::test::scan-dir arg)))

  (define (scheme::test::traverse root)
    (let ((info (scheme::test::clean-path root)))
      (walk (car info) (lambda (file)
                         (if (string-suffix? "-test.scm" file)
                             (scheme::test::run-file file (cdr info)))))))

  (define (scheme::test::scan-dir root)
    (let* ((info (scheme::test::clean-path root))
           (dir (car info)))
      (for-each (lambda (file)
                  (if (string-suffix? "-test.scm" file)
                      (scheme::test::run-file (string-append dir "/" file)
                                              (cdr info))))
                (directory-list dir))))

  (define (scheme::test::clean-path path)
    (let* ((cd (current-directory))
           (cd-len (string-length cd)))
      (case path
        (("") (cons "/" 0))
        ((".") (cons cd (+ cd-len 1)))
        (else
         (if (string-prefix? "/" path)
             (cons path (string-length path))
             (cons (string-append cd "/" path) (+ cd-len 1)))))))

  (define (scheme::test::run-file file prefix)
    (letrec ((name (substring file prefix (string-length file)))
             (lib (load file))
             (num-tests 0)
             (num-errors 0)
             (func-name "")
             (log '())
             (t (lambda (action . args)
                  (case action
                    ((test)
                     (set! num-tests (+ num-tests 1)))
                    ((eq?)
                     (if (not (eq? (car args) (cadr args)))
                         (t 'error (string-append func-name ": got") (car args)
                            "expected" (cadr args))))
                    ((error)
                     (set! num-errors (+ num-errors 1))
                     (set! log (cons args log)))))))
      (for-each (lambda (export)
                  (set! func-name (symbol->string export))
                  (if (string-prefix? "test-" func-name)
                      (scheme::test::run-test export t)
                      ))
                (caddr lib))
      (cond ((> num-errors 0)
             (display "fail "))
            ((> num-tests 0)
             (display "ok   "))
            (else
             (display "?    ")))
      (display name)
      (newline)
      (if (> num-errors 0)
          (for-each (lambda (error)
                      (display (string-join
                                (map (lambda (e) (format "~a" e)) error)
                                " "))
                      (newline))
                    (reverse log)))
      ))

  (define (scheme::test::run-test name t)
    (let ((fn (eval name (interaction-environment))))
      (if (procedure? fn)
          (begin
            (t 'test)
            (fn t)))))

  )

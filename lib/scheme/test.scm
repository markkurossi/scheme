;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (scheme test (1 0))
  (export scheme::test::runner)
  (import (rnrs base) (filepath walk))

  (define (scheme::test::runner . args)
    (if (null? args)
        (scheme::test::handle-arg ".")
        (for-each scheme::test::handle-arg args)))


  (define (scheme::test::handle-arg arg)
    (if (string-suffix? "/..." arg)
        (scheme::test::traverse (substring arg 0 (- (string-length arg) 4)))
        (scheme::test::scan-dir arg)))

  (define (scheme::test::traverse root)
    (walk (scheme::test::clean-path root) scheme::test::file-filter))

  (define (scheme::test::scan-dir root)
    (let ((dir (scheme::test::clean-path root)))
      (for-each (lambda (file)
                  (scheme::test::file-filter (string-append dir "/" file)))
                (directory-list dir))))

  (define (scheme::test::clean-path path)
    (case path
      (("") "/")
      ((".") (current-directory))
      (else
       (if (string-prefix? "/" path)
           path
           (string-append (current-directory) "/" path)))))

  (define (scheme::test::file-filter file)
    (if (string-suffix? "-test.scm" file)
        (scheme::test::run-file file)))

  (define (scheme::test::run-file file)
    (display "running test ")
    (display file)
    (newline)
    (let ((lib (load file)))
      (for-each (lambda (export)
                  (if (string-prefix? "test-" (symbol->string export))
                      (scheme::test::run-test lib export)))
                (caddr lib))))

  (define (scheme::test::run-test lib name)
    (let ((fn (eval name (interaction-environment))))
      (if (procedure? fn)
          (begin
            (display " - test ") (display name) (newline)
            (apply fn '(#t))))))
  )

;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define (load filename)
  (let* ((stack (scheme::stack-trace))
         (library (scheme::load (caadr stack) filename)))
    (scheme::init-library library)))

(define scheme::libraries '(((rnrs) initialized)))

(define (scheme::init-library library)
  (letrec ((main?
            (lambda (name)
              (equal? name '(main))))

           (join
            (lambda (items sep)
              (letrec ((head '())
                       (tail '()))
                (for-each (lambda (item)
                            (let ((p (cons item '())))
                              (if (null? tail)
                                  (set! head p)
                                  (set-cdr! tail (cons sep p)))
                              (set! tail p)))
                          items)
                head)))

           ;; The make-library-path creates an operating system file
           ;; path for the argument library name.
           (make-library-path
            (lambda (name)
              (apply string-append
                     (append
                      (join (append
                             (list (getenv "HOME")
                                   "go/src/github.com/markkurossi/scheme/lib")
                             (map symbol->string name))
                            "/")
                      '(".scm")))))

           ;; The importer imports all library imports and returns a
           ;; boolean success status.
           (importer
            (lambda (imports)
              (if (null? imports)
                  #t
                  (let* ((lib-name (car imports))
                         (lib (assoc lib-name scheme::libraries)))
                    (cond
                     ((not lib)
                      ;; Load library.
                      (load (make-library-path lib-name))

                      ;; Check that the library was initialized.
                      (set! lib (assoc lib-name scheme::libraries))
                      (if (and lib (eq? (cadr lib) 'initialized))
                          (importer (cdr imports))
                          #f))
                     ((eq? (cadr lib) 'initialized)
                      ;; Import initialized.
                      (importer (cdr imports)))
                     (else
                      ;; Dependency error.
                      #f))))))

           ;; XXX if the following closing paren is missing, VM
           ;; crashes.
           )

    (let* ((lib-name (cadr library))
           (lib-exports (caddr library))
           (lib-imports (cadddr library))
           (lib-init (cadddr (cdr library)))
           (this (assoc lib-name scheme::libraries))
           (result #f))
      (if #f
          (begin
            (display "scheme::init-library: ") (display lib-name)
            (if (not (null? lib-imports))
                (begin
                  (display " import ")
                  (display lib-imports)))
            (newline)))

      (if this
          ;; Library seen, check that is has been initialized
          ;; successfully.
          ((eq? (cadr this) 'initialized) #t)

          ;; Library not seen before. Initialize it now.
          (begin
            (set! this (list lib-name 'initializing))
            (if (not (main? lib-name))
                (set! scheme::libraries (cons this scheme::libraries)))

            ;; Imports.
            (if (importer lib-imports)
                (begin
                  ;; Imports loaded, now init this module.
                  (set! result (lib-init))
                  (set-car! (cdr this) 'initialized))
                (set-cdr! this 'error))
            result)))))

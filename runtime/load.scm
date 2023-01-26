;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(define load-path (list "/opt/scheme/share/lib"))

(let ((gohome (getenv "GOHOME"))
      (home (getenv "HOME"))
      (gopath "/src/github.com/markkurossi/scheme/lib"))
  (cond
   ((> (string-length gohome) 0)
    (set! load-path (cons (string-append gohome gopath) load-path)))
   ((> (string-length home) 0)
    (set! load-path (cons (string-append home "/go" gopath) load-path)))))

;; (display "load-path=") (display load-path) (newline)

(define (load-library name)
  (letrec ((name-string-list (map symbol->string name))
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

           ;; The make-path creates an operating system file path for
           ;; the argument library name.
           (make-path
            (lambda (dir)
              (apply string-append
                     (append (join (append (list dir) name-string-list)
                                   "/")
                             '(".scm")))))
           (iter
            (lambda (path)
              (if (null? path)
                  #f
                  (let ((filename (make-path (car path))))
                    (if (file-exists? filename)
                        (load filename)
                        (iter (cdr path))))))))
    (iter load-path)))

(define (load filename)
  (let* ((stack (scheme::stack-trace))
         (library (scheme::load (caadr stack) filename)))
    (scheme::init-library library)))

(define scheme::libraries
  '(
    ((rnrs) (6) initialized)
    ((rnrs lists) (6) initialized)
    ((rnrs files) (6) initialized)
    ((rnrs programs) (6) initialized)
    ((rnrs mutable-pairs) (6) initialized)
    ((rnrs mutable-strings) (6) initialized)
    ))

(define (scheme::init-library library)
  (letrec ((main?
            (lambda (name)
              (equal? name '(main))))

           (parse-lib-name
            (lambda (name)
              (let ((l (length name)))
                (cond
                 ((< l 2) name)
                 ((pair? (list-ref name (- l 1)))
                  (reverse (list-tail (reverse name) 1)))
                 (else name)))))

           (parse-lib-version
            (lambda (name)
              (let ((l (length name)))
                (cond
                 ((< l 2) '())
                 ((pair? (list-ref name (- l 1))) (list-ref name (- l 1)))
                 (else '())))))

           (lib-version (lambda (v) (cadr v)))
           (lib-status (lambda (v) (caddr v)))

           (set-lib-status!
            (lambda (lib status)
              (set-car! (cddr lib) status)))

           ;; The importer imports all library imports and returns a
           ;; boolean success status.
           (importer
            (lambda (imports)
              (if (null? imports)
                  #t
                  (let* ((lib-name (parse-lib-name (car imports)))
                         (lib-version (parse-lib-version (car imports)))
                         (lib (assoc lib-name scheme::libraries)))
                    (cond
                     ((not lib)
                      ;; Load library.
                      (load-library lib-name)

                      ;; Check that the library was initialized.
                      (set! lib (assoc lib-name scheme::libraries))
                      (if (and lib (eq? (lib-status lib) 'initialized))
                          (importer (cdr imports))
                          #f))
                     ((eq? (lib-status lib) 'initialized)
                      ;; Import initialized.
                      (importer (cdr imports)))
                     (else
                      ;; Dependency error.
                      #f)))))))

    (let* ((lib-name (parse-lib-name (cadr library)))
           (lib-version (parse-lib-version (cadr library)))
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
          ((eq? (lib-status this) 'initialized) #t)

          ;; Library not seen before. Initialize it now.
          (begin
            (set! this (list lib-name lib-version 'initializing))
            (if (not (main? lib-name))
                (set! scheme::libraries (cons this scheme::libraries)))

            ;; Imports.
            (if (importer lib-imports)
                (begin
                  ;; Imports loaded, now init this module.
                  (set! result (lib-init))
                  (set-lib-status! this 'initialized))
                (set-lib-status! this 'error))

            ;; XXX return an error if init fails
            result)))))

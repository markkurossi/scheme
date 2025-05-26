;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (filepath walk (1 0))
  (export walk)
  (import (rnrs base))

  (define (walk root fn)
    (letrec ((process-dir
              (lambda (directories directory files)
                (if (null? files)
                    (next-dir directories)
                    (let ((path (string-append directory "/" (car files))))
                      (if (file-directory? path)
                          (process-dir (cons path directories) directory
                                       (cdr files))
                          (begin
                            (fn path)
                            (process-dir directories directory
                                         (cdr files))))))))
             (next-dir
              (lambda (directories)
                (if (null? directories)
                    #t
                    (process-dir (cdr directories) (car directories)
                                 (directory-list (car directories)))))))
      (process-dir '() root (directory-list root))))
  )

;;;
;;; Copyright (c) 2022-2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "cond"
        (lambda (t)
          (if (not (eq? (cond ((> 3 2) 'greater)
                              ((< 3 2) 'less))
                        'greater))
              (t 'error "cond 'greater")))
        (lambda (t)
          (if (not (eq? (cond ((> 3 3) 'greater)
                              ((< 3 3) 'less)
                              (else 'equal))
                        'equal))
              (t 'error "cond 'equal")))
        (lambda (t)
          (if (not (eq? (cond ((> 3 2))
                              ((< 3 3) 'less)
                              (else 'equal))
                        #t))
              (t 'error "cond #t")))
        (lambda (t)
          (if (not (eq? (cond ('(1 2 3) => cadr)
                              (else #f))
                        2))
              (t 'error "cond =>")))
        )

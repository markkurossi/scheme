;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "append"
        (lambda (t)
          (if (not (equal? (append '(x) '(y)) '(x y)))
              (t 'error "append '(x) '(y)")))
        (lambda (t)
          (if (not (equal? (append '(a) '(b c d)) '(a b c d)))
              (t 'error "append '(a) '(b c d)")))
        (lambda (t)
          (if (not (equal? (append '(a (b)) '((c))) '(a (b) (c))))
              (t 'error "append '(a (b)) '((c))")))
        (lambda (t)
          (if (not (equal? (append '(a b) '(c . d)) '(a b c . d)))
              (t 'error "append '(a b) '(c . d)")))
        (lambda (t)
          (if (not (equal? (append '() 'a) 'a))
              (t 'error "append '() 'a")))
        )

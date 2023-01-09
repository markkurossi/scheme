;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "and"
        (lambda (t)
          (if (not (eq? (and) #t))
              (t 'error "(and)")))
        (lambda (t)
          (if (not (eq? (and (= 2 2) (> 2 1)) #t))
              (t 'error "(and #t)")))
        (lambda (t)
          (if (not (eq? (and (= 2 2) (< 2 1)) #f))
              (t 'error "(and #f)")))
        (lambda (t)
          (if (not (equal? (and 1 2 'c '(f g)) '(f g)))
              (t 'error "(and '(f g))")))
        )

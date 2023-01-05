;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "odd?"
        (lambda (t)
          (if (not (odd? 1))
              (t 'error "odd? 1")))
        (lambda (t)
          (if (not (odd? #e1))
              (t 'error "odd? #e1")))
        )

(runner 'run "even?"
        (lambda (t)
          (if (not (even? 0))
              (t 'error "even? 0")))
        (lambda (t)
          (if (not (even? 42))
              (t 'error "even? 42")))
        (lambda (t)
          (if (not (even? #e42))
              (t 'error "even? #e42")))
        )

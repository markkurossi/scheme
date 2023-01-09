;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "case"
        (lambda (t)
          (if (not (eq? (case (* 2 3)
                          ((2 3 5 7) 'prime)
                          ((1 4 6 8 9) 'composite))
                        'composite))
              (t 'error "case 'composite")))
        (lambda (t)
          (if (not (eq? (case (car '(c d))
                          ((a) 'a)
                          ((b) 'b))
                        #f))
              (t 'error "case #f")))
        (lambda (t)
          (if (not (eq? (case (car '(c d))
                          ((a e i o u) 'vowel)
                          ((w y) 'semivowel)
                          (else 'consonant))
                        'consonant))
              (t 'error "case 'consonant")))
        )

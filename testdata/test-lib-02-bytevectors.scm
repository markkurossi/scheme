;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "2. Bytevectors")

(runner 'run "bytevector?"
        (lambda (t)
          (if (not (bytevector? #vu8(12 23 123)))
              (t 'error "bytevector?")))
        )
(runner 'run "bytevector-length"
        (lambda (t)
          (if (not (= (bytevector-length #vu8()) 0))
              (t 'error "bytevector-length")))
        (lambda (t)
          (if (not (= (bytevector-length #vu8(12 23 123)) 3))
              (t 'error "bytevector-length")))
        )
(runner 'run "bytevector=?"
        (lambda (t)
          (if (not (bytevector=? #vu8() #vu8()))
              (t 'error "bytevector=? #vu8()")))
        (lambda (t)
          (if (not (bytevector=? #vu8(1 2 3) #vu8(1 2 3)))
              (t 'error "bytevector=? #vu8(1 2 3)")))
        (lambda (t)
          (if (bytevector=? #vu8(1 2 3) #vu8(1 2 4))
              (t 'error "bytevector=? #vu8(1 2 3) #vu8(1 2 4)")))
        (lambda (t)
          (if (bytevector=? #vu8(1 2 3) #vu8())
              (t 'error "bytevector=? #vu8(1 2 3) #vu8()")))
        )

(runner 'run "bytevector-copy"
        (lambda (t)
          (if (not (bytevector=? (bytevector-copy #vu8()) #vu8()))
              (t 'error "bytevector-copy")))
        (lambda (t)
          (if (not (bytevector=? (bytevector-copy #vu8(1 2 3)) #vu8(1 2 3)))
              (t 'error "bytevector-copy")))
        )

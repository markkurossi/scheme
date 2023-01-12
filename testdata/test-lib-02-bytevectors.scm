;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "2. Bytevectors")

(runner 'test "bytevector?"
        (lambda () (bytevector? #vu8(12 23 123)))
        )
(runner 'test "bytevector-length"
        (lambda () (= (bytevector-length #vu8()) 0))
        (lambda () (= (bytevector-length #vu8(12 23 123)) 3))
        )
(runner 'test "bytevector=?"
        (lambda () (bytevector=? #vu8() #vu8()))
        (lambda () (bytevector=? #vu8(1 2 3) #vu8(1 2 3)))
        (lambda () (not (bytevector=? #vu8(1 2 3) #vu8(1 2 4))))
        (lambda () (not (bytevector=? #vu8(1 2 3) #vu8())))
        )

(runner 'test "bytevector-copy"
        (lambda () (bytevector=? (bytevector-copy #vu8()) #vu8()))
        (lambda () (bytevector=? (bytevector-copy #vu8(1 2 3)) #vu8(1 2 3)))
        )

;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "2. Bytevectors")

(runner 'test "bytevector?"
        (lambda () (bytevector? #vu8(12 23 123)))
        )
(runner 'test "make-bytevector"
        (lambda () (bytevector=? (make-bytevector 0) #vu8()))
        (lambda () (bytevector=? (make-bytevector 5) #vu8(0 0 0 0 0)))
        (lambda () (bytevector=? (make-bytevector 5 42) #vu8(42 42 42 42 42)))
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
(runner 'test "bytevector-fill"
        (lambda () (bytevector=? (bytevector-fill (make-bytevector 5) 42)
                                 #vu8(42 42 42 42 42)))
        )
(runner 'test "bytevector-copy!"
        (lambda () (let ((source (make-bytevector 5 42))
                         (target (make-bytevector 5 32)))
                     (bytevector-copy! source 2
                                       target 3 2)
                     (bytevector=? target #vu8(32 32 32 42 42))))
        )

(runner 'test "bytevector-copy"
        (lambda () (bytevector=? (bytevector-copy #vu8()) #vu8()))
        (lambda () (bytevector=? (bytevector-copy #vu8(1 2 3)) #vu8(1 2 3)))
        )

(runner 'test "bytevector-{u,s}8-ref"
        (lambda() (equal? (let ((b1 (make-bytevector 16 -127))
                                (b2 (make-bytevector 16 255)))
                            (list
                             (bytevector-s8-ref b1 0)
                             (bytevector-u8-ref b1 0)
                             (bytevector-s8-ref b2 0)
                             (bytevector-u8-ref b2 0)))
                          (list -127 129 -1 255)))
        )

;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'test "any->scheme"
        (lambda () (eq? (any->scheme '()) "'()"))
        (lambda () (eq? (any->scheme #t) "#t"))
        (lambda () (eq? (any->scheme #f) "#f"))
        (lambda () (eq? (any->scheme 42) "42"))
        (lambda () (eq? (any->scheme #\a) "#\\a"))
        (lambda () (eq? (any->scheme 'foo) "foo"))
        (lambda () (eq? (any->scheme '(a)) "(a)"))
        (lambda () (eq? (any->scheme '(a b)) "(a b)"))
        (lambda () (eq? (any->scheme '(a b . c)) "(a b . c)"))
        (lambda () (eq? (any->scheme "") "\"\""))
        (lambda () (eq? (any->scheme "foo") "\"foo\""))
        (lambda () (eq? (any->scheme "foo\"") "\"foo\\\"\""))
        (lambda () (eq? (any->scheme "foo\\") "\"foo\\\\\""))

        (lambda () (eq? (any->scheme '#()) "#()"))
        (lambda () (eq? (any->scheme '#(1)) "#(1)"))
        (lambda () (eq? (any->scheme '#(1 2)) "#(1 2)"))

        (lambda () (eq? (any->scheme #vu8()) "#vu8()"))
        (lambda () (eq? (any->scheme #vu8(1)) "#vu8(1)"))
        (lambda () (eq? (any->scheme #vu8(1 2)) "#vu8(1 2)"))
        )

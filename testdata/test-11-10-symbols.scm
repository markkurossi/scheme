;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'sub-section "11.10. Symbols")

(runner 'test "symbol?"
        (lambda () (symbol? 'foo))
        (lambda () (symbol? (car '(a b))))
        (lambda () (not (symbol? "bar")))
        (lambda () (symbol? 'nil))
        (lambda () (not (symbol? '())))
        (lambda () (not (symbol? #f)))
        )

(runner 'test "symbol->string"
        (lambda () (string=? (symbol->string 'flying-fish)
                             "flying-fish"))
        (lambda () (string=? (symbol->string 'Martin)
                             "Martin"))
        (lambda () (string=? (symbol->string (string->symbol "Malvina"))
                             "Malvina"))
        )

(runner 'test "symbol=?"
        (lambda () (symbol=? 'a 'a))
        (lambda () (symbol=? 'a 'a 'a))
        )

(runner 'test "string->symbol"
        (lambda () (not (eq? 'mISSISSIppi 'mississippi)))
        (lambda () (symbol=? (string->symbol "mISSISSIppi")
                             'mISSISSIppi))
        (lambda () (eq? 'bitBlt (string->symbol "bitBlt")))
        (lambda () (eq? 'JollyWog
                        (string->symbol
                         (symbol->string 'JollyWog))))
        (lambda () (string=? "K. Harper, M.D."
                             (symbol->string
                              (string->symbol "K. Harper, M.D."))))
        )

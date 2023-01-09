;;;
;;; Copyright (c) 2023 Markku Rossi
;;;
;;; All rights reserved.
;;;

(runner 'run "symbol?"
        (lambda (t)
          (if (not (symbol? 'foo))
              (t 'error "symbol? 'foo")))
        (lambda (t)
          (if (not (symbol? (car '(a b))))
              (t 'error "symbol? (car '(a b))")))
        (lambda (t)
          (if (symbol? "bar")
              (t 'error "symbol? \"bar\"")))
        (lambda (t)
          (if (not (symbol? 'nil))
              (t 'error "symbol? 'nil")))
        (lambda (t)
          (if (symbol? '())
              (t 'error "symbol? '()")))
        (lambda (t)
          (if (symbol? #f)
              (t 'error "symbol? #f")))
        )

(runner 'run "symbol->string"
        (lambda (t)
          (if (not (string=? (symbol->string 'flying-fish)
                             "flying-fish"))
              (t 'error "symbol->string 'flying-fish")))
        (lambda (t)
          (if (not (string=? (symbol->string 'Martin)
                             "Martin"))
              (t 'error "symbol->string \"Martin\"")))
        (lambda (t)
          (if (not (string=? (symbol->string (string->symbol "Malvina"))
                             "Malvina"))
              (t 'error "string->symbol->string \"Malvina\"")))
        )

(runner 'run "symbol=?"
        (lambda (t)
          (if (not (symbol=? 'a 'a))
              (t 'error "symbol=? 'a 'a")))
        (lambda (t)
          (if (not (symbol=? 'a 'a 'a))
              (t 'error "symbol=? 'a 'a 'a")))
        )

(runner 'run "string->symbol"
        (lambda (t)
          (if (eq? 'mISSISSIppi 'mississippi)
              (t 'error "eq? 'mISSISSIppi 'mississippi")))
        (lambda (t)
          (if (not (symbol=? (string->symbol "mISSISSIppi")
                             'mISSISSIppi))
              (t 'error "string->symbol \"mISSISSIppi\"")))
        (lambda (t)
          (if (not (eq? 'bitBlt (string->symbol "bitBlt")))
              (t 'error "eq? 'bitBlt")))
        (lambda (t)
          (if (not (eq? 'JollyWog
                        (string->symbol
                         (symbol->string 'JollyWog))))
              (t 'error "eq? symbol->string->symbol 'JollyWog")))
        (lambda (t)
          (if (not (string=? "K. Harper, M.D."
                             (symbol->string
                              (string->symbol "K. Harper, M.D."))))
              (t 'error "string=? string->symbol->string \"K. Harper, M.D.\"")))
        )

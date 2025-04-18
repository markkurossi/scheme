(import (html escape))

(pragma (verbose-typecheck #t))

(define msg "<&foo&>")

(display (html-escape msg)) (newline)

(display (html-unescape (html-escape msg))) (newline)

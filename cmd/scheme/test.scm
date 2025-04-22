(import (html unescape))

(pragma (verbose-typecheck #f))

(define (test-entities pos)
  (if (>= pos (vector-length html-entities))
      #t
      (let* ((entity (vector-ref html-entities pos))
             (input (string-append "&" (car entity) " "))
             (result (html-unescape input)))
        (if (> (string-length result) 3)
            (begin
              (display "entity ") (display pos) (display ": ")
              (display input) (display "=> ") (display result)
              (newline)))
        (test-entities (+ pos 1)))))

(test-entities 0)

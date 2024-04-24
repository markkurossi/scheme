(define (leibniz rounds)
  (set! rounds (+ rounds 2))
  (letrec ((i 2)
           (x 1.0)
           (pi 1.0)
           (loop (lambda ()
                   (if (< i rounds)
                       (begin
                         (set! x (* x -1))
                         (set! pi (+ pi (/ x (- (* i 2) 1))))
                         (set! i (+ i 1))
                         (loop))))))

    (loop)
    (* pi 4)))

(display (leibniz 100000000))
(newline)

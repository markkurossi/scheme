;; Prime Number Calculator in Scheme
;; Various functions for computing and working with prime numbers

(import (rnrs lists))

;; Helper function: check if a number divides another evenly
(define (divides? a b)
  (= (remainder b a) 0))

;; Basic primality test using trial division
(define (prime? n)
  (cond
    ((< n 2) #f)                    ; numbers less than 2 are not prime
    ((= n 2) #t)                    ; 2 is prime
    ((even? n) #f)                  ; even numbers > 2 are not prime
    (else (prime-helper n 3))))     ; check odd divisors starting from 3

;; Helper function for prime? - tests divisors up to sqrt(n)
(define (prime-helper n divisor)
  (cond
    ((> (* divisor divisor) n) #t)           ; if divisor > sqrt(n), n is prime
    ((divides? divisor n) #f)                ; if divisor divides n, not prime
    (else (prime-helper n (+ divisor 2)))))  ; try next odd divisor

;; Generate a list of all primes up to n using trial division
(define (primes-up-to n)
  (letrec ((collect-primes
            (lambda (current acc)
              (cond
               ((> current n) (reverse acc))
               ((prime? current) (collect-primes (+ current 1)
                                                 (cons current acc)))
               (else (collect-primes (+ current 1) acc))))))
    (collect-primes 2 '())))

;; Generate the first n prime numbers
(define (first-n-primes n)
  (letrec ((collect-n-primes
            (lambda (count current acc)
              (cond
               ((= count 0) (reverse acc))
               ((prime? current) (collect-n-primes (- count 1) (+ current 1)
                                                   (cons current acc)))
               (else (collect-n-primes count (+ current 1) acc))))))
    (collect-n-primes n 2 '())))

;; Sieve of Eratosthenes implementation
(define (sieve-of-eratosthenes n)
  ;; Create initial list of candidates from 2 to n
  (letrec ((make-candidates
            (lambda (start end)
              (if (> start end)
                  '()
                  (cons start (make-candidates (+ start 1) end)))))

           ;; Remove multiples of prime p from the list
           (remove-multiples
            (lambda (p lst)
              (filter (lambda (x) (or (= x p) (not (divides? p x)))) lst)))

           ;; Main sieve function
           (sieve
            (lambda (candidates)
              (cond
               ((null? candidates) '())
               ((> (* (car candidates) (car candidates)) n) candidates)
               (else
                (let ((p (car candidates)))
                  (cons p (sieve (remove-multiples p (cdr candidates))))))))))

    (if (< n 2)
        '()
        (sieve (make-candidates 2 n)))))

;; Find the next prime number after n
(define (next-prime n)
  (letrec ((search (lambda (current)
                     (if (prime? current)
                         current
                         (search (+ current 1))))))
    (search (+ n 1))))

;; Check if two numbers are twin primes (differ by 2)
(define (twin-primes? p q)
  (and (prime? p)
       (prime? q)
       (= (abs (- p q)) 2)))

;; Generate list of twin prime pairs up to n
(define (twin-primes-up-to n)
  (letrec ((collect-twins
            (lambda (current acc)
              (cond
               ((> (+ current 2) n) (reverse acc))
               ((and (prime? current) (prime? (+ current 2)))
                (collect-twins (+ current 2)
                               (cons (list current (+ current 2)) acc)))
               (else (collect-twins (+ current 1) acc))))))
    (collect-twins 3 '())))

;; Factorize a number into prime factors
(define (prime-factors n)
  (letrec ((factor-helper
            (lambda (n divisor acc)
              (cond
               ((= n 1) (reverse acc))
               ((divides? divisor n) (factor-helper (/ n divisor) divisor
                                                    (cons divisor acc)))
               ((= divisor 2) (factor-helper n 3 acc))
              (else (factor-helper n (+ divisor 2) acc))))))
    (if (< n 2)
        '()
        (factor-helper n 2 '()))))

;; Test the functions
(define (run-tests)
  (display "Testing prime number functions:\n")

  (display "prime? tests: ")
  (display (list (prime? 2) (prime? 17) (prime? 25) (prime? 97)))
  (newline)

  (display "First 10 primes: ")
  (display (first-n-primes 10))
  (newline)

  (display "Primes up to 30: ")
  (display (primes-up-to 30))
  (newline)

  (display "Sieve of Eratosthenes up to 30: ")
  (display (sieve-of-eratosthenes 30))
  (newline)

  (display "Next prime after 20: ")
  (display (next-prime 20))
  (newline)

  (display "Twin primes up to 30: ")
  (display (twin-primes-up-to 30))
  (newline)

  (display "Prime factors of 60: ")
  (display (prime-factors 60))
  (newline))

;; Run the tests
(run-tests)

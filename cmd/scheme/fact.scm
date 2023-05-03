
(define (fact n)
  (if (< n #e2)
      #e1
      (* n (fact (- n #e1)))))

(display
 (fact #e100))
(newline)
;;;
;;; (* #e2 (if (< (- #e2 #e1) #e2) #e1))

#lang htdp/isl+

;; Exercise 465. Design subtract. The function consumes two Equations
;; of equal length. It “subtracts” a multiple of the second equation
;; from the first, item by item, so that the resulting Equation has a
;; 0 in the first position. Since the leading coefficient is known to
;; be 0, subtract returns the rest of the list that results from the
;; subtractions.

(define E1 '(4 1 -2 1))
(define E2 '(2 2 3 10))

; Equation Equation -> Equation
(check-expect (subtract '(2 2) '(2 5)) '(-3))
(check-expect (subtract '(2 2 3 10) '(2 5 12 31)) '(-3 -9 -21))
(check-expect (subtract '(3 9 21) '(-3 -8 -19)) '(1 2))
(check-expect (subtract E1 E2) '(-3 -8 -19))
(check-expect (subtract '(3  9  21) '(-3 -8 -19)) '(1 2))
(define (subtract e1 e2)
  (cond
    [(not (= (length e1) (length e2))) (error "Err Length")]
    [else (local ((define multiple (/ (first e1) (first e2))))
            (if (integer? multiple)
                (map (lambda (x y) (- x (* multiple y)))
                     (rest e1) (rest e2))
                (error "Err Remainder")))]))

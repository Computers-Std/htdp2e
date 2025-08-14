#lang htdp/isl+

; An SOE is an non-empty Matrix.
; constraint: for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation: represents a system of linear equations

; An Equation is a [List-of Number]
; constraint: an Equation contains at least two numbers
; interpretation: if (list a1 ... an b) is an Equation, a1 ,..., an
; are the left-hand-side variable coefficients and b is the right-hand
; side

; A Solution is a [List-of Number]

(define M                               ; an SOE
  (list (list 2 2 3 10)                 ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define S '(1 1 2))                     ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
(check-expect (check-solution M S) #true)
(define (check-solution soe sol)
  (local ((define lhs-list (map lhs soe))
          (define rhs-list (map rhs soe))
          (define val-list
            (foldr (lambda (lh val)
                     (cons (plugin lh sol) val)) '() lhs-list)))
    (equal? val-list rhs-list)))

; [List-of Number] Solution -> Number
(check-expect (plugin '(2 2 3) '(1 1 2)) 10)
(check-expect (plugin '(2 5 12) '(1 1 2)) 31)
(define (plugin loc sol)
  (foldr (lambda (a b total) (+ (* a b) total)) 0 loc sol))

#lang htdp/isl+

; An Equation is a [List-of Number]

; An SOE is an non-empty Matrix.
(define M (list (list 2 2 3 10)
                (list 2 5 12 31)
                (list 4 1 -2 1)))

; A Solution is a [List-of Number]
(define S '(1 1 2))

; Gaussian Elimination Matrix
(define GEM (list (list 2 2 3 10)
                  (list 0 3 9 21)
                  (list 0 0 1 2)))

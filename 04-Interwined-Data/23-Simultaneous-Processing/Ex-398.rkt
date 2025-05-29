#lang htdp/isl+

(define coeffs '(5 17 3))
(define vals '(10 1 2))

; [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (value coeffs vals) 73)
(define (value loc lov)
  (cond
    [(or (empty? loc) (empty? lov)) 0]
    [else (+ (* (first loc) (first lov))
             (value (rest loc) (rest lov)))]))

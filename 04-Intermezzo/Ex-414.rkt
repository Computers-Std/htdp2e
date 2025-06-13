#lang htdp/isl+

(define x #i1/185)

; Number -> Number
(check-within (add 0) x 0.0001)
(check-within (add 1) (+ x x) 0.0001)
(check-within (add 185) 1 0.01)
(check-within (add 1000) 5.4 0.1)
(define (add n)
  (cond
    [(= n 0) x]
    [else (+ x (add (sub1 n)))]))

; Number -> Number
(check-expect (sub (* 3 x)) 3)
(check-expect (sub x) 1)
(check-expect (sub 2/185) 2)
(check-expect (sub 1) 185)
(check-expect (sub #i1.0) 185)
(define (sub arg)
  (cond
    [(> x arg) 0]
    [else (add1 (sub (- arg x)))]))

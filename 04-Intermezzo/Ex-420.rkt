#lang htdp/isl+

(define (oscillate n)
  (local ((define (O i)
            (cond
              [(> i n) '()]
              [else (cons (expt #i-0.99 i) (O (+ i 1)))])))
    (O 1)))


; [List-of Number] -> Number
(define (sum lon)
  (foldl + 0 lon))

(sum (oscillate #i1000.0))
; #i-0.49746596003269394

(sum (reverse (oscillate #i1000.0)))
; #i-0.4974659600326953

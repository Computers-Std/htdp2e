#lang htdp/isl+

; Number Number Posn -> Number
; computes distance b/w (x, y) and Posn p
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))

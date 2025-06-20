#lang htdp/isl+

; N[>= 1] N[>= 1] -> N
; find the greatest common divisor of n and m
(define (gcd-structural n m)
  (local ((define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else (if (= (remainder n i) (remainder m i) 0)
                        i
                        (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

(time (gcd-structural 101135853 45014640))

;; cpu time: 1168 real time: 1170 gc time: 1
;
;; 177

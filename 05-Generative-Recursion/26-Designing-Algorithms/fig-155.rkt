#lang htdp/isl+

(define (gcd-generative n m)
  (local (; N[>= 1] N -> N
          ; Generative Recursion
          ; (gcd L S) == (gcd S (remainder L S))
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

(time (gcd-generative 101135853 45014640))

;; cpu time: 0 real time: 0 gc time: 0
;; 177

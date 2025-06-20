#lang htdp/isl+

;; Exercise 438. In your words: how does greatest-divisor-<= work? Use
;; the design recipe to find the right words. Why does the locally
;; defined greatest-divisor-<= recur on (min n m)?

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

(check-expect (gcd-structural 25 6) 1)

;; (= (remainder 25 6) (remainder 6 6))    ; #false
;; (= (remainder 25 5) (remainder 6 5))    ; #false
;; (= (remainder 25 4) (remainder 6 4))    ; #false
;; (= (remainder 25 3) (remainder 6 3))    ; #false
;; (= (remainder 25 2) (remainder 6 2))    ; #false
;; (= (remainder 25 1) (remainder 6 1))    ; #true

; Answer :: The function greatest-divisor-<= takes the minimum of the
; two numbers to reduce the number of steps. Either one of the numbers
; will be the same. It divides and checks the remainder of the smaller
; number by the other one, decrements and continues until 1 if no
; common divisor for both numbers is found.

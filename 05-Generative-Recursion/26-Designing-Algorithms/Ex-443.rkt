#lang htdp/isl+

;; Exercise 443. Given the header material for gcd-structural, a naive
;; use of the design recipe might use the following template or some
;; variant:

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(define (gcd-structural n m)
  (cond
    [(and (= n 1) (= m 1)) ...]
    [(and (> n 1) (= m 1)) ...]
    [(and (= n 1) (> m 1)) ...]
    [else
     (... (gcd-structural (sub1 n) (sub1 m)) ...
          ... (gcd-structural (sub1 n) m) ...
          ... (gcd-structural n (sub1 m)) ...)]))

;; Why is it impossible to find a divisor with this strategy?

#| As we are dealing with divisors, we do not need to iterate through
each number down to 0. Instead, we should divide the numbers by the
appropriate divisor and then make the next recursive call with the
resulting numbers.  |#

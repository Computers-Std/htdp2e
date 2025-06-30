#lang htdp/isl+

(define EPSILON 0.001)
(define ERR-ZERO-SLOPE "The slope is 0.")

; Number -> Number
(define (poly x) (* (- x 2) (- x 4)))

; [Number -> Number] Number -> Number
; finds a number r such that (<= (abs (f r)) ε)
(check-within (newton poly 1) 2 EPSILON)
(check-within (newton poly 3.5) 4 EPSILON)
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPSILON) r1]
    [else (newton f (root-of-tangent f r1))]))

; see exercise 455
(define (slope f r1)
  (local
      ((define r0 (- r1 EPSILON))
       (define r2 (+ r1 EPSILON))
       (define f@r0 (f r0))
       (define f@r2 (f r2)))
    (/ (- f@r2 f@r0) (* 2 EPSILON))))

; see exercise 456
(define (root-of-tangent f r1)
  (local ((define slope-value (slope f r1)))
    (if (zero? slope-value)
        (error ERR-ZERO-SLOPE)
        (- r1 (/ (f r1) slope-value)))))

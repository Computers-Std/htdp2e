#lang htdp/isl+

(define EPSILON 0.001)
(define ERR-ZERO-SLOPE "The slope is 0.")

(define (poly x)
  (* (- x 2) (- x 1)))

; [Number -> Number] Number -> Number
(check-expect (slope poly 1) -1)
(check-expect (slope poly 3) 3)
(check-expect (slope poly 1.5) 0)
(define (slope f r1)
  (local
      ((define r0 (- r1 EPSILON))
       (define r2 (+ r1 EPSILON))
       (define f@r0 (f r0))
       (define f@r2 (f r2)))
    (/ (- f@r2 f@r0) (* 2 EPSILON))))

; [Number -> Number] Number -> Number
(check-expect (root-of-tangent poly 1) 1)
(check-expect (root-of-tangent poly 3) 7/3)
(check-error (root-of-tangent poly 1.5) ERR-ZERO-SLOPE)
(define (root-of-tangent f r1)
  (local ((define slope-value (slope f r1)))
    (if (zero? slope-value)
        (error ERR-ZERO-SLOPE)
        (- r1 (/ (f r1) slope-value)))))

#lang htdp/isl+

(define EPSILON 0.001)

(define (poly x)
  (* (- x 2) (- x 1)))

(define f1 (lambda (x) (+ (* x x) x -2)))

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

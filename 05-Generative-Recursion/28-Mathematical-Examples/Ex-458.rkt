#lang htdp/isl+

(define EPSILON 0.01)

; [Number -> Number] Number Number -> Number
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 EPSILON)

;; (check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
;;               1000
;;               EPSILON)
;; (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) => 1500

(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (trapezoid-area l r)
            (* 0.5 (- r l) (+ (f l) (f r)))))
    (+ (trapezoid-area a mid)
       (trapezoid-area mid b))))

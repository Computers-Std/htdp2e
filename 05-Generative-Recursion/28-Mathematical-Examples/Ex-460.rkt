#lang htdp/isl+

(define EPSILON 0.01)

;; [Number -> Number] Number Number -> Number
;; Computes the area under the graph of f between a and b.
;; Assumes that (< a b) holds.
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (trapezoid-area l r)
            (* 0.5 (- r l) (+ (f l) (f r)))))
    (+ (trapezoid-area a mid)
       (trapezoid-area mid b))))

;; [Number -> Number] Number Number -> Number
;; Computes the area under the graph of f between a and b.
;; Assumes that (< a b) holds.
(define (integrate-dc f a b)
  (local ((define mid (* 0.5 (+ a b)))
          (define delta (- b a)))
    (cond
      [(<= delta EPSILON) (integrate-kepler f a b)]
      [else (+ (integrate-dc f a mid)
               (integrate-dc f mid b))])))

(check-within (integrate-dc (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

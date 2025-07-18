#lang htdp/isl+
;; Exercise 461. Design integrate-adaptive. That is, turn the
;; recursive process description into an ISL+ algorithm
(define EPSILON 0.1)

; [Number -> Number] Number Number -> Number
; Computes the area under the graph of f between a and b
; Assumes that (< a b) holds.
(define (integrate-adaptive f a b)
  (local
      ((define mid (/ (+ a b) 2))
       (define (trapezoid-area l r)
         (* 0.5 (- r l) (+ (f l) (f r))))
       (define trapezoid-l (trapezoid-area a mid))
       (define trapezoid-r (trapezoid-area mid b)))
    (cond
      [(< (abs (- trapezoid-r trapezoid-l))
          (* EPSILON (abs (- b a))))
       (+ trapezoid-l trapezoid-r)]
      [else (+ (integrate-adaptive f a mid)
               (integrate-adaptive f mid b))])))

(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

#lang htdp/isl+

;; Exercise 432. Exercise 219 introduces the function food-create,
;; which consumes a Posn and produces a randomly chosen Posn that is
;; guaranteed to be distinct from the given one. First reformulate the
;; two functions as a single definition, using local; then justify the
;; design of food-create.


(define MAX 10)

;; [List-of Posn] -> Posn
(define (food-create lop)
  (local ((define candidate (make-posn (random MAX) (random MAX))))
    (if (member? candidate lop) (food-create lop) candidate)))

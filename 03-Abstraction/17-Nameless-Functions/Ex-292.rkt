#lang htdp/isl+

;; Exercise 292. Design the function sorted?, which comes with the
;; following signature and purpose statement:

; [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (sorted? cmp alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) #true]
    [(cmp (first alon) (first (rest alon)))
     (sorted? cmp (rest alon))]
    [else #false]))

;; (define (sorted? cmp l)
;;   (local ((define (check2 x y)
;;             (cmp x y)))
;;     (andmap check2 l)))

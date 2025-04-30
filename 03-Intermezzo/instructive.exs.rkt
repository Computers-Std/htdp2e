#lang htdp/isl+

(require 2htdp/abstraction)

(define (my-for/and i)
  (local ((define (1arg-func? arg)
            (> (- 9 arg) 0)))
    (andmap 1arg-func? (build-list i (lambda (n) n)))))

(define (and-map func ls)
  (for/and ([i ls])
    (if (func i) i #false)))

; Sequence
(define (enumerate.v2 lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))

; N -> Number
; adds the even numbes between 0 and n (exclusive)
(define (sum-evens n)
  (for/sum ([i (in-range 0 n 2)]) i))

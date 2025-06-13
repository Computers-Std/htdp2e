#lang htdp/isl+

; Number Number -> Number
; Raises the n to the power of p
(define (my-expt n p)
  (cond
    [(zero? p) 1]
    [else (* n (my-expt n (sub1 p)))]))

(define inex (+ 1 #i1e-12))
(define exac (+ 1 1e-12))

; The inex one is more useful as it gives a concise output in
; Approximated Inexact form.

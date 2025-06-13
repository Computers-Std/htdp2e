#lang htdp/isl+

;; Exercise 415. ISL+ uses +inf.0 to deal with overflow. Determine the
;; integer n such that
;; (expt #i10.0 n)
;; is an inexact number while (expt #i10. (+ n 1)) is approximated
;; with +inf.0. Hint Design a function to compute n.

; N is one of:
; - 0
; - (add1 N)

; N -> N
(define (compute n)
  (cond
    [(not (integer? (expt #i10.0 n))) (- n 1)]
    [else (compute (add1 n))]))

(compute 0) ; 308

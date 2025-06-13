#lang htdp/isl+

; N is one of:
; - 0
; - (add1 N)

; N -> N
(define (compute n)
  (cond
    [(equal? #i0.0 (expt #i10.0 n)) (add1 n)]
    [else (compute (sub1 n))]))

(compute 0) ; -323

(expt #i10. -323)
; #i9.8813129168249e-324
(expt #i10. -324)
; #i0.0

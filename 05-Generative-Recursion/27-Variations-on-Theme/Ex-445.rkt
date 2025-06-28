#lang htdp/isl+

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

;; [Number -> Number] Number Number -> Number
;; determines R such that f has a root in [R, (+ R EPSILON)]
;; Assume that
;  - f is continuous
;  - (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
;; Divides the interval in half, the root is in one of the two halves,
;; picks according to (2)
(check-within (find-root poly 1 3) 2 0.001)
(check-within (find-root poly 3 5) 4 0.001)
(check-satisfied (find-root poly 3 5)
                 (lambda (n) (zero? (poly (round n)))))
(define (find-root f left right) 0)


;; ???

#lang htdp/isl+

(define EPSILON 0.001)

;; Number -> Number
;; Defines a binomial with two roots: 2 and 4.
(check-expect (poly 2) 0)
(check-expect (poly 4) 0)
(define (poly x)
  (* (- x 2) (- x 4)))

;; [Number -> Number] Number Number -> Number
;; determines R such that f has a root in [R, (+ R EPSILON)]
;; Assume that
;  - f is continuous
;  - (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
;; Divides the interval in half, the root is in one of the two halves,
;; picks according to assumption
(check-within (find-root poly 1 3) 2 EPSILON)
(check-within (find-root poly 3 5) 4 EPSILON)
(check-satisfied (find-root poly 3 5)
                 (lambda (n) (zero? (poly (round n)))))
(define (find-root f left right)
  (cond
    [(<= (- right left) EPSILON) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

#lang htdp/isl+

;; Exercise 450. A function f is monotonically increasing if (<= (f a)
;; (f b)) holds whenever (< a b) holds. Simplify find-root assuming
;; the given function is not only continuous but also monotonically
;; increasing

(define EPSILON 0.001)

;; Number -> Number
;; Defines a binomial with two roots: 2 and 4.
;; f(x) = (x - 2) (x - 4)
(define (poly x)
  (* (- x 2) (- x 4)))

;; [Number -> Number] Number Number -> Number
;; determines R such that f has a root in [R, (+ R EPSILON)]
;; Assume that
;  - f is continuous
;  - (<= (f l) (f r)) i.e monotonous
;; Divides the interval in half, the root is in one of the two halves,
;; picks according to assumption
(check-within (find-root poly 3 5) 4 EPSILON)
(check-within (find-root poly 1 3) 2 EPSILON)
(check-satisfied (find-root poly 1 7) (lambda (n) (zero? (poly (round n)))))
(check-satisfied (find-root poly 2 5) (lambda (n) (zero? (poly (round n)))))

(define (find-root f left right)
  (local ((define (root-helper l r)
            (if (<= (- r l) EPSILON) l
                (local ((define mid (/ (+ l r) 2))
                        (define f@mid (f mid)))
                  (cond
                    [(zero? f@mid) mid]
                    [(<= f@mid 0) (root-helper l mid)]
                    [else (root-helper mid r)])))))
    (root-helper left right)))

;; Monotonic: In simpler terms, as the input xx increases, the output
;; of the function f(x) never decreases. It either stays the same or
;; increases, but never decreases.

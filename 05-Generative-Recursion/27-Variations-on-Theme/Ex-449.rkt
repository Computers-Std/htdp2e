#lang htdp/isl+

(define EPSILON 0.001)
(define ERROR-INTERVAL "In every recursive call, f(left) & f(right) must be on Opp. sides of x-axis")

;; Number -> Number
;; Defines a binomial with two roots: 2 and 4.
;; f(x) = (x - 2) (x - 4)
(define (poly x)
  (* (- x 2) (- x 4)))

;; [Number -> Number] Number Number -> Number
;; determines R such that f has a root in [R, (+ R EPSILON)]
;; Assume that
;  - f is continuous
;  - (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
;; Divides the interval in half, the root is in one of the two halves,
;; picks according to assumption
(check-error (find-root poly 1 8) ERROR-INTERVAL)
(check-within (find-root poly 3 5) 4 EPSILON)
(check-satisfied (find-root poly 1 7) (lambda (n) (zero? (poly (round n)))))

(define (find-root f left right)
  (local ((define (root-helper l r f@left f@right)
            (local ((define mid (/ (+ l r) 2))
                    (define f@mid (f mid)))
              (if (<= (- r l) EPSILON)
                  l
                  (cond [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
                         (root-helper l mid f@left f@mid)]
                   [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
                    (root-helper mid r f@mid f@right)]
                   [else (error ERROR-INTERVAL)])))))
    (root-helper left right (f left) (f right))))

;; Note the two additional args (f@left, f@right) to the helper
;;function change at each recursive stage, but the chage is related to
;;the change in numeric args(l, r). These args are so-called
;;`accumulators`, topic of "Part-VI Accumulators"

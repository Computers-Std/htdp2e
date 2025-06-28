#lang htdp/isl+

(define EPSILON 0.001)
;; (define ERROR-INTERVAL "f(mid) does not yeild in between f(left) and f(right)")
(define ERROR-INTERVAL "In every recursive call, f(left) & f(right) must be on Opp. sides of x-axis")

;; Number -> Number
;; Defines a binomial with two roots: 2 and 4.
;; f(x) = (x - 2) (x - 4)
;; f(x) = x^2 - 6x + 8
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
(check-error (find-root poly 1 9) ERROR-INTERVAL)
(check-within (find-root poly 1 3) 2 EPSILON)
(check-error (find-root poly 1 8) ERROR-INTERVAL)
(check-within (find-root poly 3 5) 4 EPSILON)
(check-satisfied (find-root poly 1 7) (lambda (n) (zero? (poly (round n)))))
(check-satisfied (find-root poly 3 5) (lambda (n) (zero? (poly (round n)))))

(define (find-root f left right)
  (cond
    [(<= (- right left) EPSILON) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid))
             (define f@left (f left))
             (define f@right (f right)))
       (cond
         [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left)) (find-root f left mid)]
         [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid)) (find-root f mid right)]
         [else (error ERROR-INTERVAL)]))]))

;; In interval [a,b]: if (a - b) = S1, then for every recursive the
;; distance will be halved, like (a1 - b1) = S1/2; (a2 - b2) = S1/4

;; After how many steps is (- right left) smaller than or equal to ε
;; [27-06-2025] FIXME: Dont Know

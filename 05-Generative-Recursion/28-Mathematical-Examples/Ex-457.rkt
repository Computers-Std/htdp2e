#lang htdp/isl+

;; Exercise 457. Design the function double-amount, which computes how
;; many months it takes to double a given amount of money when a
;; savings account pays interest at a fixed rate on a monthly basis.

; Number -> Number
; A = P (1 + r)^n
; For how many months, will A = 2P
; interpretation: r is rate-of-interest, n is no-of-months
; solve for n in 2 = (1 + r)^n

;; N is one of:
;; - 0
;; - (add1 N)

(define AMOUNT-UNIT 100)

;; N[>=1 <=100] -> N[>=1]
(define (double-amount r)
  (local ((define month-rate (+ 1 (/ r 100)))
          (define doubled (* 2 AMOUNT-UNIT))
          ;; N -> N
          (define (add-rate amount)
            (round (* month-rate amount)))
          ;; N N -> N
          (define (double-amount* a m)
            (cond
              [(>= a doubled) m]
              [else (double-amount* (add-rate a) (add1 m))])))
    (double-amount* (add-rate AMOUNT-UNIT) 1)))


; NOTE: The program is not mine 😞

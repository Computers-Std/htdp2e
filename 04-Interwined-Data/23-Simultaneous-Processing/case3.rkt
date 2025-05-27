#lang htdp/isl+

;; Sample Problem: Given a list of symbols los and a natural number n,
;; the function list-pick extracts the nth symbol from los; if there
;; is no such symbol, it signals an error.

(define ERROR "list too short")

; N is one of:
; - 0
; - (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; signals an error if there is no such
; symbol
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l)) (error ERROR)]
    [(and (> n 0) (empty? l)) (error ERROR)]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (sub1 n) (rest l))]))

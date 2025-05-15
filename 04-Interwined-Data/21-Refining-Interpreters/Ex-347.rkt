#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
; (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
; (make-mul BSL-expr BSL-expr)

; BSL-expr is one of
; - Number
; - String
; - Boolean
; - Image
; - BSL-expr

; BSL-expr -> BSL-expr
(define (eval-expression bexp)
  (cond
    [(number? bexp) bexp]
    [(add? bexp) (plus (eval-expression (add-left bexp))
                       (eval-expression (add-right bexp)))]
    [(mul? bexp) (multiply (eval-expression (mul-left bexp))
                           (eval-expression (mul-right bexp)))]))

; Number Number -> Number
(define (plus l r)
  (cond
    [(= r 0) l]
    [(= l 0) r]
    [else (plus (add1 l) (sub1 r))]))

; Number Number -> Number
(define (multiply l r)
  (cond
    [(or (= l 0) (= r 0)) 0]
    [else (plus l (multiply l (sub1 r)))]))

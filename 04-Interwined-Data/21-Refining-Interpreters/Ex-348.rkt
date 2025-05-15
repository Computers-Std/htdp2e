#lang htdp/isl+

(define-struct bor [left right])
; (make-bor BSL-expr BSL-expr)

(define-struct band [left right])
; (make-band BSL-expr BSL-expr)

(define-struct bnot [val])
; (make-bnot BSL-expr)

; Here BSL-expr is one of:
; - #false
; - #true
; - BSL-expr

; BSL-expr BSL-expr -> BSL-
(check-expect (eval-bor #false #false) #false)
(check-expect (eval-bor #false #true) #true)
(check-expect (eval-bor #true #true) #true)
(check-expect (eval-bor #true #false) #true)
(define (eval-bor l r)
  (cond
    [(true? l) l]
    [(true? r) r]
    [else l]))

; BSL-expr -> BSL-expr
(define (true? bool)
  (eval-bnot (false? bool)))

; BSL-expr -> BSL-expr
(define (eval-bnot bexp)
  (if (false? bexp) #true #false))

; BSL-expr BSL-expr -> BSL-expr
(check-expect (eval-band #false #false) #false)
(check-expect (eval-band #false #true) #false)
(check-expect (eval-band #true #true) #true)
(check-expect (eval-band #true #false) #false)
(define (eval-band l r)
  (cond
    [(true? l) (true? r)]
    [else l]))

; BSL-expr BSL-expr -> BSL-expr
(check-expect (eval-bool-expr (make-bor #false #true)) #true)
(check-expect (eval-bool-expr (make-band #true #true)) #true)
(check-expect (eval-bool-expr (make-bnot #true)) #false)
(define (eval-bool-expr bexp)
  (cond
    [(bor? bexp) (eval-bor (bor-left bexp) (bor-right bexp))]
    [(band? bexp) (eval-band (band-left bexp) (band-right bexp))]
    [(bnot? bexp) (eval-bnot bexp)]
    [else bexp]))

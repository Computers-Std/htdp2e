#lang htdp/isl+

;; Exercise 352. Design subst. The function consumes a BSL-var-expr
;; ex, a Symbol x, and a Number v. It produces a BSL-var-expr like ex
;; with all occurrences of x replaced by v.

(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-var-expr BSL-var-expr)
;; (make-add expr1 expr2) represents
;; a BSL expression for addition of expr1 and expr2.

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-var-expr BSL-var-expr)
;; (make-mul expr1 expr2) represents
;; a BSL expression for multiplication of expr1 and expr2.

; Any -> Boolean
(define (atom? n)
  (or (number? n) (symbol? n)))

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst (make-add 3 'x) 'x 5) (make-add 3 5))
(check-expect (subst (make-mul 1/2 (make-mul 'x 5)) 'x 5) (make-mul 1/2 (make-mul 5 5)))
(define (subst ex x v)
  (cond
    [(atom? ex) (if (equal? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))

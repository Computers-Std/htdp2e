#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; An AL (association list) is [List-of Association]
; An Association is a list of two items:
; (cons Symbol (cons Number '()))

(define WRONG "Invalid Expression provided.")

; Any -> Boolean
(define (atom? n)
  (or (number? n) (symbol? n)))

; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst (make-add 3 'x) 'x 5) (make-add 3 5))
(check-expect (subst (make-mul 1/2 (make-mul 'x 5)) 'x 5) (make-mul 1/2 (make-mul 5 5)))
(define (subst ex x v)
  (cond
    [(atom? ex) (if (equal? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]))

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr
(check-expect (numeric? (make-add 3 4)) #true)
(check-expect (numeric? (make-add 3 'x)) #false)
(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex)) (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex)) (numeric? (mul-right ex)))]
    [else #false]))

;; BSL-expr -> BSL-value
;; Computes value of the given BSL-expr.
(check-expect (eval-expression 10) 10)
(check-expect (eval-expression (make-add (make-mul 20 3) 33)) 93)
(check-expect (eval-expression (make-mul (make-add 20 (make-add 10 10)) (make-mul 3 2))) 240)
(define (eval-expression ex)
  (cond
    [(number? ex) ex]
    [(add? ex) (+ (eval-expression (add-left ex)) (eval-expression (add-right ex)))]
    [(mul? ex) (* (eval-expression (mul-left ex)) (eval-expression (mul-right ex)))]))

;; BSL-var-expr -> BSL-value
(check-error (eval-variable (make-mul 1/2 (make-mul 'x 3))) WRONG)
(check-expect (eval-variable (make-add (make-mul 1 2) (make-add 10 2))) 14)
(check-error (eval-variable (make-add (make-mul 'x 'x) (make-add 'y 'y))) WRONG)
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error WRONG)))

; BSL-var-expr AL -> [Maybe BSL-var-expr]
; Starting from ex, it iteratively applies subst to all associations
; in da
(check-expect (eval-variable* (make-add 'x 'y) '((x 3) (y 4))) 7)
(check-expect (eval-variable* (make-mul 'x 'y) '((x 3) (y 4))) 12)
(check-expect (eval-variable* (make-mul 1/2 (make-mul 'y 4)) '((y 4))) 8)
(define (eval-variable* ex da)
  (cond
    [(empty? da) (eval-variable ex)]
    [else (eval-variable* (subst ex (first (first da)) (second (first da))) (rest da))]))

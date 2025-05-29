#lang htdp/isl+

;; Exercise 354. Design eval-variable. The checked function consumes a
;; BSL-var-expr and determines its value if numeric? yields true for
;; the input. Otherwise it signals an error.

;; In general, a program defines many constants in the definitions
;; area, and expressions contain more than one variable. To evaluate
;; such expressions, we need a representation of the definitions area
;; when it contains a series of constant definitions. For this
;; exercise we use association lists:

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

;; Make up elements of AL.
;; Design eval-variable*. The function consumes a BSL-var-expr ex and
;; an association list da. Starting from ex, it iteratively applies
;; subst to all associations in da. If numeric? holds for the result,
;; it determines its value; otherwise it signals the same error as
;; eval-variable.

;; Hint: Think of the given BSL-var-expr as an atomic value and
;; traverse the given association list instead. We provide this hint
;; because the creation of this function requires a little design
;; knowledge from Simultaneous Processing.

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
(check-expect (eval-variable* (make-add 'x 'y)
                              '((x 3) (y 4))) 7)
(check-expect (eval-variable* (make-mul 'x 'y)
                              '((x 3) (y 4))) 12)
(check-expect (eval-variable* (make-mul 1/2 (make-mul 'y 4))
                              '((y 4))) 8)
(define (eval-variable* ex da)
  (cond
    [(empty? da) (eval-variable ex)]
    [else (eval-variable*
           (subst ex (first (first da))
                  (second (first da)))
           (rest da))]))

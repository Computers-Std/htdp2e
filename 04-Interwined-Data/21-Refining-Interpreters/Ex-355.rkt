#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; An AL (association list) is [List-of Association]
; An Association is a list of two items:
; (cons Symbol (cons Number '()))

(define WRONG "Invalid Expression")

; BSL-var-expr AL -> Number
(check-expect (eval-var-lookup (make-add 'x (make-mul 3 'y)) '((x 2) (y 4))) 14)
(check-expect (eval-var-lookup (make-mul (make-add 'x 'y) (make-mul 'y 3)) '((x 5) (y 10))) 450)
(define (eval-var-lookup e da)
  (local ((define (resolve sym pairs)
            (local ((define val (assq sym pairs)))
              (if (false? val)
                  (error WRONG)
                  (second val)))))
    (cond
      [(symbol? e) (resolve e da)]
      [(number? e) e]
      [(add? e) (+ (eval-var-lookup (add-left e) da)
                   (eval-var-lookup (add-right e) da))]
      [(mul? e) (* (eval-var-lookup (mul-left e) da)
                   (eval-var-lookup (mul-right e) da))])))

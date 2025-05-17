#lang htdp/isl+

;; Exercise 353. Design the numeric? function. It determines whether a
;; BSL-var-expr is also a BSL-expr. Here we assume that your solution
;; to exercise 345 is the definition for BSL-var-expr without Symbols.

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

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr
(check-expect (numeric? (make-add 3 4)) #true)
(check-expect (numeric? (make-add 3 'x)) #false)
(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]
    [else #false]))

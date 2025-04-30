#lang htdp/isl+

;; (define x (cons 1 x))
; The shaded occurence of x bound to nothing, as the function header
; expects no argument

; something like this
(define (y x) (cons 1 x))

; as we only deal with Constant definitions something that is defined
; before or a known value like '()
(define x (cons 1 '()))

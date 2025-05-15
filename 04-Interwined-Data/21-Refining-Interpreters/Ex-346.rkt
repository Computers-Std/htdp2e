#lang htdp/isl+

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

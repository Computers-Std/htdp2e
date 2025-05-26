#lang htdp/isl+

; An Xexpr is a list:
; (cons Symbol Body)

; where Body is a list:
; - '()
; - (cons Xexpr Body)
; - (cons [List-of Attribute] Body)

; An Attribute is a list of two items:
; (cons Symbol (cons String '()))

#lang htdp/isl+

; An Xexpr is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))

; A Body is one of:
; - [List-of Xexpr]
; - XWord

; An Attribute is a list of two items:
; (cons Symbol (cons String '()))

; An XWord is '(word ((text String)))

;; -- ^^ ) --

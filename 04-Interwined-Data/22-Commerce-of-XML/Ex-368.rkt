#lang htdp/isl+

; An Attrs-or-Xexpr is one of:
; - [List-of Attribute]
; - Xexpr

; Attrs-or-Xexpr -> Boolean
; Is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

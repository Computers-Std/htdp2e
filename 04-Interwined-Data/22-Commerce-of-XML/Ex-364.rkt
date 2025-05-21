#lang htdp/isl+

; An Xexpr is a list:
; (cons Symbol Body)

; where Body is a list:
; - '()
; - (cons Xexpr Body)
; - (cons [List-of Attribute] Body)

; An Attribute is a list of two items:
; (cons Symbol (cons String '()))

; 1. <transition from="seen-e" to="seen-f" />
'(transaction ((from "seen-e") (to "seen-f")))

; 2. <ul><li><word /><word /></li><li><word /></li></ul>
'(ul (li (word) (word)) (li (word)))

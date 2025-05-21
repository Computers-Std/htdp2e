#lang htdp/isl+

; An Xexpr.v2 is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) ...]
      [else (... (first optional-loa+content) ...
                 ... (xexpr-attr (rest optional-loa+content)) ...)])))

; The finised parsing function does not require a self-reference of
; xexpr-attr, as the List-of Attributes, Can only be present in the
; second position in list. It will redundant to traverse entire list
; for LOA.

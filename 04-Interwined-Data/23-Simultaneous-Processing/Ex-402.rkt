#lang htdp/isl+

;; Exercise 402. Reread exercise 354. Explain the reasoning behind our
;; hint to think of the given expression as an atomic value at first.

; When the function is given two complex inputs, and only one of them
; could be a list, it falls under the first category described in
; "Simultaneous Processing", which states:

; If one of the parameters plays a dominant role, think of the other
; as an atomic piece of data as far as the function is concerned.

; With this principle, we will traverse the list while treating the
; other input as an atomic value

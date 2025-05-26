#lang htdp/isl+
;; Exercise 381. The definitions of XMachine and X1T use quote, which
;; is highly inappropriate for novice program designers. Rewrite them
;; first to use list and then cons

; FSM-State is a Color.

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))

; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; =====

; An XMachine is a nested list of this shape:
; (list 'machine (list (list 'initial "red")) [List-of X1T])

; An XMachine is a nested list of this shape:
; (cons 'machine (cons (cons (cons 'initial (cons "red" '())) '()) [List-of X1T]))

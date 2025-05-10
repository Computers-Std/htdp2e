#lang htdp/isl+

(define-struct child [father mother name date eyes])
(define-struct no-parent [])
(define NP (make-no-parent))

; An FT is one of:
; - NP
; - (make-child FT FT String N String)

;; Fig 112: Sample Family Tree

; Oldest Gen
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Gen
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1950 "black"))
(define Eva (make-child Carl Bettina "Eva" 1950 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Gen
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Number
; counts the child structures in the tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 7)
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ 1
             (count-persons (child-mother an-ftree))
             (count-persons (child-mother an-ftree)))]))

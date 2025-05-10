#lang htdp/isl+

(define-struct no-parent [])
(define-struct child [father mother name date eyes])

; An FT (short for family tree) is one of:
; - (make-no-parent)
; - (make-child FT FT String N String)

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

;; Figure 113
; FT-> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

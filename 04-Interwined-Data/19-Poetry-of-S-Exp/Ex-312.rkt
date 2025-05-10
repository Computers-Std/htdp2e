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

; FT -> [List-of String]
; produces a list of eye-colors in a FT
(check-expect (eye-colors Carl)
              (list "green"))
(check-expect (eye-colors Gustav)
              (list "brown" "blue" "green" "green" "pink"))
(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]         ; or empty-string ""
    [else (append (list (child-eyes an-ftree))
                  (eye-colors (child-mother an-ftree))
                  (eye-colors (child-father an-ftree)))]))

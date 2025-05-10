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

; FT -> Number
; counts average of all child structures in the FT
(check-expect (average-age Eva 2000) 66)
(check-expect (average-age Carl 2000) 74)
(define (average-age an-ftree present)
  (local ((define (count-persons an-ftree)
            (cond
              [(no-parent? an-ftree) 0]
              [else (+ 1
                       (count-persons (child-mother an-ftree))
                       (count-persons (child-mother an-ftree)))]))
          (define (total-age an-ftree)
            (cond
              [(no-parent? an-ftree) 0]
              [else (+ (- present (child-date an-ftree))
                       (total-age (child-mother an-ftree))
                       (total-age (child-father an-ftree)))])))
    (/ (total-age an-ftree) (count-persons an-ftree))))

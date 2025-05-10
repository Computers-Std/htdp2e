#lang htdp/isl+

(define-struct child [father mother name date eyes])
(define-struct no-parent [])
(define NP (make-no-parent))

; An FT is one of:
; - NP
; - (make-child FT FT String N String)

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

; FT -> Boolean
; was there any ancestor with blue eyes
(check-expect (blue-eyed-ancestor? Carl) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(check-expect (blue-eyed-ancestor? Eva) #false)

;; (define (blue-eyed-ancestor? an-ftree)
;;   (cond
;;     [(no-parent? an-ftree) #false]
;;     [else (or (blue-eyed-ancestor? (child-mother an-ftree))
;;               (blue-eyed-ancestor? (child-father an-ftree)))]))

(define (blue-eyed-ancestor? an-ftree)
  (local ((define (got-blue-eyes? child)
            (and (child? child)
                 (string=? (child-eyes child) "blue"))))
    (cond
      [(no-parent? an-ftree) #false]
      [else (or (got-blue-eyes? (child-mother an-ftree))
                (got-blue-eyes? (child-father an-ftree))
                (blue-eyed-ancestor? (child-father an-ftree))
                (blue-eyed-ancestor? (child-mother an-ftree)))])))

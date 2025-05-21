#lang htdp/isl+

; An Xexpr.v2 is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))

(define e2 '(machine (action)))
(define e3 '(machine () (action))) ; should be equivalent to e2

(define e4 `(machine ,a0 (action) (action)))
; e4: <machine initial="X"><action /><action /></machine>

; Xexpr.v2 -> Symbol
; retrieves the name (symbol) of xe
(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Xexpr.v2]
; retrieves the list-of content of xe
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(define (xexpr-content xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else
       (local ((define loa-or-content (first opt-loa+content))
               (define possible-content (rest opt-loa+content)))
         (if (list-of-attributes? loa-or-content)
             possible-content opt-loa+content))])))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list-of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

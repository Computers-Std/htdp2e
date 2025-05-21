#lang htdp/isl+

;; '(machine ((initial "red"))
;;           (action ((state "red") (next "green")))
;;           (action ((state "green") (next "yellow")))
;;           (action ((state "yellow") (next "red"))))

;; (list 'machine
;;       (list (list 'initial "red"))
;;       (list 'action (list (list 'state "red") (list 'next "green")))
;;       (list 'action (list (list 'state "green") (list 'next "yellow")))
;;       (list 'action (list (list 'state "yellow") (list 'next "red"))))

; An Xexpr.v0 (short for X-expression) is one-item list:
; (cons Symbol '())

; An Xexpr.v1 is a list:
; (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
; (cons Symbol (cons String '()))



(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))

(define e2 '(machine (action)))
(define e3 '(machine () (action))) ; should be equivalent to e2

(define e4 `(machine ,a0 (action) (action)))
; e4: <machine initial="X"><action /><action /></machine>

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list-of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(define (xexpr-attr xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else
       (local ((define loa-or-x (first opt-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

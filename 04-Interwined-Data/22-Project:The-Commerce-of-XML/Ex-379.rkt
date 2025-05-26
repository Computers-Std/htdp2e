#lang htdp/isl+

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;  (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; Data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "orange") "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

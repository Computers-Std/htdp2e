#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;  (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; Data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; FSM-State FSM -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw
             (lambda (current) (square 100 "solid" current))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))

(define (simulate-1 state0 transitions)
  (big-bang state0
            [to-draw
             (lambda (current)
               (above (text current 40 current)
                      (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))

; Application
;; (simulate "green" fsm-traffic)
;; (simulate-1 "green" fsm-traffic)

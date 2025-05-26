#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

; Exercise 380. Reformulate the data definition for 1Transition so
; that it is possible to restrict transitions to certain keystrokes.
; Try to formulate the change so that find continues to work without
; change. What else do you need to change to get the complete program
; to work? Which part of the design recipe provides the answer(s)? See
; exercise 229 for the original exercise statement.

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
; (cons (cons Initial-State (cons Key-Event '())) (cons Final-State))
; Initial-State and Final-State are FSM-States
; An FSM-State is a String that specifies a color

; Data examples
(define fsm-traffic
  '((("red" "g") "green") (("green" "y") "yellow") (("yellow" "r") "red")))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic '("green" "y")) "yellow")
(check-error (find fsm-traffic '("orange" "z")) "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm)
        (second fm)
        (error "not found"))))

; FSM-State FSM -> FSM-State
(define (simulate state0 transitions)
  (big-bang state0
            [to-draw
             (lambda (current)
               (overlay (text current 20 "black")
                        (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               ;; [24-05-2025] NOTE: why it dont work
               ;; (find transitions '(current key-event))
               (find transitions (cons current (cons key-event '()))))]))

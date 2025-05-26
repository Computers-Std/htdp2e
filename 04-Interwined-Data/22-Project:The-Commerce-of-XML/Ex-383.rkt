#lang htdp/isl+
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)
;; Sample Problem: Design a program that uses an XMachine configuration
;; to run simulate

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))
; FSM-State is a Color.

(define bwm
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; XMachine -> FSM-State
; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; XMachine -> FSM-State
; extracts and translates the transition table from xm
(check-expect (xm-state0 xm0) "red")
(define (xm-state0 xm)
  (find-attr (xexpr-attr xm) 'initial))

; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; FSM-State FSM -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw
             (lambda (current)
               (square 100 "solid" current))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "orange") "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm)
        (second fm)
        (error "not found"))))

; Xexpr -> Xexpr
(define (xexpr-content xe)
  (local ((define opt-loa+content (rest xe))
          (define possible-content (rest opt-loa+content)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (if (list-of-attributes? (first opt-loa+content))
                possible-content opt-loa+content)])))

; [Maybe [List-of Attribute]] -> Boolean
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr -> [List-of Attribute]
;; (check-exp)
(define (xexpr-attr xe)
  (local ((define opt-loa+content (rest xe)))
    (cond
      [(empty? opt-loa+content) '()]
      [else (local ((define loa-or-xe (first opt-loa+content)))
              (if (list-of-attributes? loa-or-xe)
                  loa-or-xe '()))])))

; [List-Of-Attribute] Attribute -> String
; produces the value of attribute
(define (find-attr loa attr)
  (local ((define val (assoc attr loa)))
    (if (false? val)
        #false (second val))))

; Application
;; (simulate-xmachine bwm)

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-228) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
; - '()
; - (cons Transition FSM)

; interpretation: an FSM represents the transitions that a finite
; state machine can take from one state to another in reaction to
; keystrokes

(define-struct transition [current next])
; a Transition is a structure:
;  (make-transition FSM-State FSM-State)

; FSM-State is a Color.

(define-struct fs [fsm current])
; A SimulationState.v2 is a Structure
; (make-fs FSM FSM-State)

;;; Constants
;; (define s0 "red")
(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; FSM-State FSM-State -> Boolean
; an equality predicate for FSM states
(check-expect (state=? "red" "green") #false)
(check-expect (state=? "green" "green") #true)
(define (state=? s1 s2)
  (string=? s1 s2))

; SimulationState.v2 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; find the next state from an-fsm and ke
(check-expect (find-next-state (make-fs fsm-traffic "red") "n")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "red") "a")
              (make-fs fsm-traffic "green"))
(define (find-next-state an-fsm ke)
  (make-fs (fs-fsm an-fsm)
           (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black") "not found: black")
(define (find trasitions current)
  (cond
    [(empty? trasitions) (error "not found: " current)]
    [else (if (state=? (transition-current (first trasitions))
                       current)
              (transition-next (first trasitions))
              (find (rest trasitions) current))]))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))

;; (simulate.v2 fsm-traffic "red")

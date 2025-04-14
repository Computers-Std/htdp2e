#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fig-102) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate fsm s0)
  (local (; State of the World: FSM-State
          ; FSM-State KeyEvent -> FSM-State
          (define (find-next-state s key)
            (find fsm s)))
    (big-bang s0
              [to-draw state-as-colored-square]
              [on-key find-next-state])))

; FSM-State -> Image
; renders current state as colored square
(define (state-as-colored-square s)
  (square 100 "solid" s))

; FSM FSM-State -> FSM-State
; finds the current state's successor in fsm
(define (find transitions current)
  (cond
    [(empty? transitions) (error "not found")]
    [else (local ((define s (first transitions)))
            (if (state=? (transition-current s) current)
                (transition-next s)
                (find (rest transitions) current)))]))

; FSM-State FSM-State -> Boolean
; an equality predicate for FSM states
(define (state=? s1 s2)
  (string=? s1 s2))

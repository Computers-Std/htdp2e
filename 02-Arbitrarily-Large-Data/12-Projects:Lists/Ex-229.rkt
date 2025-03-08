#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-229) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
; (make-ktransition FSM-State KeyEvent FSM-State)

; FSM-State is a Color.

(define-struct fs [fsm current])
; A SimulationState.v2 is a Structure
; (make-fs FSM FSM-State)

;;; Constants

(define fsm-109
  (list (make-ktransition "white" "a" "yellow")
        (make-ktransition "yellow" "b" "yellow")
        (make-ktransition "yellow" "c" "yellow")
        (make-ktransition "yellow" "d" "green")))

;; Functions

; FSM-State FSM-State -> Boolean
; an equality predicate for FSM states
(define (state=? s1 s2)
  (string=? s1 s2))

; SimulationState.v2 -> Image
; renders current world state as a colored square
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; find the next state from an-fsm and ke
(define (find-next-state.v2 an-fsm key)
  (make-fs (fs-fsm an-fsm) (find.v2 (fs-fsm an-fsm) (fs-current an-fsm) key)))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find.v2 fsm-109 "white" "n") "white")
(check-expect (find.v2 fsm-109 "yellow" "b") "yellow")
(check-expect (find.v2 fsm-109 "yellow" "d") "green")
(define (find.v2 ktransitions current key)
  (cond
    [(empty? ktransitions) current]
    [else
     (if (and (state=? (ktransition-current (first ktransitions)) current)
              (key=? (ktransition-key (first ktransitions)) key))
         (ktransition-next (first ktransitions))
         (find.v2 (rest ktransitions) current key))]))

;; (define (find.v3 ktransitions current key)
;; (cond
;; [(empty? ktransitions) current]
;; [(and (state=? (ktransition-current (first ktransitions)) current)
;; (key=? (ktransition-key (first ktransitions)))) (if )]))

; white a -> yellow
; white b,c,d -> red
; yellow b,c -> yellow
; yellow d -> green
; yellow a -> red

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state.v2]))

;; (simulate.v2 fsm-traffic "red")


;; Testing Area
;; ============
(define example-keys (explode "abcdefghijklmnopqrstuvwxyz"))

; List-of-letters -> List-of-letters
; remove the List-of-letters from allKeys and produce new List-of-letters
(check-expect (any-but-keys (list "a" "b")) (explode "cdefghijklmnopqrstuvwxyz"))
(define (any-but-keys ls)
  (cond
    [(empty? ls) example-keys]
    [else
     (if (member? (first ls) example-keys)
         (remove (first ls) (any-but-keys (rest ls)))
         (any-but-keys (rest ls)))]))

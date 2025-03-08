#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-230) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

;; Data Definitions

;; Q: Represent the FSM from exercise 109 in this context.

(define-struct fsm [initial transitions final])
; An FSM is a structure:
;   (make-fsm FSM-State LOT FSM-State)

; A LOT is one of:
; - '()
; - (cons Transition LOT)

(define-struct transition [current key next])
; A Transition is a structure:
;   (make-transition FSM-State KeyEvent FSM-State)

(define transitions-109
  (list (make-transition "white" "a" "yellow")
        (make-transition "yellow" "b" "yellow")
        (make-transition "yellow" "c" "yellow")
        (make-transition "yellow" "d" "green")
        ;; (make-transition "green" "a" "white") ; cycle
        ))

;; Ex-109
; white a -> yellow
; white b,c,d -> red
; yellow a -> red
; yellow b,c -> yellow
; yellow d -> green

(define fsm-109 (make-fsm "white" transitions-109 "green"))

;;; Functions

; FSM-State FSM-State -> Boolean
; an equality predicate for FSM states
(define (state=? s1 s2)
  (string=? s1 s2))

; SimulationState.v2 -> Image
; renders current world state as a colored square
(define (render-state an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

; FSM KeyEvent -> FSM
(check-expect (find-next-state fsm-109 "a") (make-fsm "yellow" transitions-109 "green"))
(check-expect (find-next-state (make-fsm "yellow" transitions-109 "green") "d")
              (make-fsm "green" transitions-109 "green"))
(define (find-next-state an-fsm key)
  (make-fsm (find (fsm-initial an-fsm) key (fsm-transitions an-fsm))
            (fsm-transitions an-fsm)
            (fsm-final an-fsm)))

; State KeyEvent Transition -> State
(check-expect (find "white" "n" transitions-109) "white")
(check-expect (find "yellow" "b" transitions-109) "yellow")
(check-expect (find "yellow" "d" transitions-109) "green")
(define (find current key transitions)
  (cond
    [(empty? transitions) current]
    [else
     (if (and (state=? (transition-current (first transitions)) current)
              (key=? (transition-key (first transitions)) key))
         (transition-next (first transitions))
         (find current key (rest transitions)))]))

; FSM -> Boolean
(check-expect (end?
               (find-next-state (make-fsm "yellow" transitions-109 "green") "d")) #true)
(define (end? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

; fsm-simulate
; FSM -> SimulationState
; match the keys pressed with the given FSM
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
            [to-draw render-state]
            [on-key find-next-state]))

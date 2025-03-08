#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
;; (require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current next])
; a Transition is a structure:
;  (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation: an FSM represents teh transitions that a finite
; state machine can take from one state to another in reaction to
; keystrokes

; FSM-State FSM-State -> Boolean
; an equality predicate for FSM states
(check-expect (state=? "red" "green") #false)
(check-expect (state=? "green" "green") #true)
(define (state=? s1 s2)
  (string=? s1 s2))

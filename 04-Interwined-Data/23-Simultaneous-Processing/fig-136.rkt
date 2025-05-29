#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

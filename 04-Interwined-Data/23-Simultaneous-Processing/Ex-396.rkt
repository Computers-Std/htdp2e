#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

;; An HM-Word is a [List-of Letter or "_"]
;; "_" represents a letter to be guessed.

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

;; Word Word 1String -> Word
;; Produces Word with the revealed letters
(check-expect (compare-word '("c" "a" "t") '("_" "_" "_") "b") '("_" "_" "_"))
(check-expect (compare-word '("c" "a" "t") '("_" "_" "_") "a") '("_" "a" "_"))
(check-expect (compare-word '("a" "b" "a") '("_" "b" "_") "a") '("a" "b" "a"))
(define (compare-word word s current-guess)
  (cond
    [(empty? word) '()]
    [else (cons (if (string=? (first word) current-guess)
                    current-guess
                    (first s))
                (compare-word (rest word) (rest s) current-guess))]))


;; Out of Context
; Letter [List-of Letter] -> Number
(check-expect (str-ref "a" '("c" "a" "t")) 1)
(define (str-ref l lol)
  (local ((define (placeof s lol n)
            (cond
               [(member? s lol)
                (if (string=? s (first lol))
                    n
                    (placeof s (rest lol) (add1 n)))]
               [else (error "not found")])))
    (placeof l lol 0)))

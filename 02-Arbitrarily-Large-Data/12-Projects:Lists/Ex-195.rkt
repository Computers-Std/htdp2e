#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-195) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

; A Dictionary is a List-of-strings.
(define LOC "/usr/share/dict/words")

(define AS-LIST (read-lines LOC))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; consumes a Letter and Dictionary, produces the count of the
; Letter'ed words in Dictionary
(check-expect (start-with# "a" dict1) 2)
(check-expect (start-with# "b" dict1) 2)
(check-expect (start-with# "b" dict1) 2)
(define (start-with# le dict)
  (cond
    [(empty? dict) 0]
    [else (if (found? le (first dict))
              (add1 (start-with# le (rest dict)))
              (start-with# le (rest dict)))]))

; Letter Dictionary -> Boolean
(check-expect (found? "a" (first dict1)) #true)
(check-expect (found? "b" (first dict1)) #false)
(define (found? le word)
  (string=? le (first (explode word))))

;; -- Testing Area
(define dict1 (list "advil" "advil" "begean" "begean"
                    "cAelfric" "cAelfric" "dAeneas"
                    "dAeneas" "eAeneid" "eAeneid"))

; In my computer dictionary:

; No. of words start with "e"
(start-with# "e" AS-LIST) ;; 4025

; No. of words start with "z"
(start-with# "z" AS-LIST) ;; 190

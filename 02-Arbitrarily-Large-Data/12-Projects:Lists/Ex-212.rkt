#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-212) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 212. Write down the data definition for List-of-words.
;; Make up examples of Words and List-of-words. Finally, formulate the
;; functional example from above with check-expect. Instead of the
;; full example, consider working with a word of just two letters, say
;; "d" and "e".

; A Word is one of:
; '()
; (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words (low) is one of:
; '()
; (cons Word List-of-words)
; interpretation a Low is a List of Words

(define word0 '())
(define word1 (list "h" "e" "l" "l" "o"))
(define word2 (list "w" "o" "r" "l" "d"))
(define word3 (list "k" "i" "r" "a" "n"))

(define low0 (list '()))
(define low1 (list word1))
(define low2 (list word1 word2))
(define low3 (list word1 word2 word3))

(check-expect word1 (cons "h" (cons "e" (cons "l" (cons "l" (cons "o" '()))))))
(check-expect low1 (cons word1 '()))

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-211) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

;; Exercise 211. Complete the design of in-dictionary, specified in
;; figure 78. Hint See Real-World Data: Dictionaries for how to read a
;; dictionary.

(define LOC "/usr/share/dict/words")
(define DICTIONARY (read-lines LOC))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary (list "hello" "cat" "tac")) (list "hello" "cat"))
(check-expect (in-dictionary (list "world" "")) (list "world"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) DICTIONARY)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))

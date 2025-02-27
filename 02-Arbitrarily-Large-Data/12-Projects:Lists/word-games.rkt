#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-games) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Sample Problem Given a word, find all words that are made up from
; the same letters. For example “cat” also spells “act.”

; List-of-Strings -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w)
   (member? "art" w)
   (member? "tar" w)))

; String -> List-of-Strings
; find all words that the letters of some given word spell

(check-member-of (alt-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))

(check-satisfied (alt-words "rat")
                 all-words-from-rat?)

(define (alt-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; List-of-words -> List-of-Strings
; turns all Words in Low into Strings
(define (words->strings low) '())

; List-of-Strings -> List-of-Strings
; picks out all those Strings that occur in the dictionary
(define (in-dictionary los) '())

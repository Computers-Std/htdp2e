#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-209) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; A Word is one of:
; '()
; (cons 1String Word)

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))
(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")
(define (word->string w)
  (cond
    [(empty? w) ""]
    [else (string-append (first w) (word->string (rest w)))]))

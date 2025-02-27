#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-210) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; A Word is one of:
; '()
; (cons 1String Word)

; A Low is one of:
; '()
; (cons Word List-of-words)

; Word -> String
; converts w to a string
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")
(define (word->string w)
  (cond
    [(empty? w) ""]
    [else (string-append (first w) (word->string (rest w)))]))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings
               (list (explode "hello") (explode "world")))
              (list "hello" "world"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

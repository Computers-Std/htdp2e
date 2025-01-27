#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-171) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)


; String -> List-of-list-of-string
; produces the content of file f as a list of list of
; strings, one list per line and one string per word
;; (define (read-words/line f) ...)
(read-words/line "poem.txt")

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-239) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; A [List X Y] is a list:
;  (cons X (cons Y '()))

; pairs of Numbers
; (cons Number (cons Number '()))
(cons 3 (cons 4 '()))

; pairs of Numbers and 1Strings
; (cons Number (cons 1Strings '()))
(cons 3 (cons "t" '()))

; pair of Strings and Booleans
; (cons String (cons Boolean '()))
(cons "Hello" (cons "#true" '()))

#lang htdp/isl+
(require 2htdp/abstraction)

;; Exercise 370. Make up three examples for XWords. Design word?,
;; which checks whether some ISL+ value is in XWord, and word-text,
;; which extracts the value of the only attribute of an instance of
;; XWord.

; An XWord is '(word ((text String)))

(define xw1 '(word ((text "Apple"))))
(define xw2 '(word ((text "Basket"))))
(define xw3 '(word ((text "Country"))))

;; Any -> Boolean
;; Determines whether v is an XWord.
(check-expect (word? '()) #false)
(check-expect (word? '(word ((text 2)))) #false)
(check-expect (word? xw1) #true)
(check-expect (word? xw2) #true)
(check-expect (word? xw3) #true)
(define (word? v)
  (match v
    [(list 'word (list (list 'text (? string?)))) #true]
    [else #false]))

; XWord -> String
; extracts the value of XWord
(check-expect (word-text xw1) "Apple")
(check-expect (word-text xw2) "Basket")
(check-expect (word-text xw3) "Country")
(define (word-text xw)
  (match xw
    [(list 'word (list (list 'text str))) str]))

; <ul>
;    <li>
;       <word />
;       <word />
;    </li>
;    <li>
;       <word />
;    </li>
; </ul>

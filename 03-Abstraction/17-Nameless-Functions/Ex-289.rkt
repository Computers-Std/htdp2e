#lang htdp/isl+

;; Exercise 271. Use ormap to define find-name. The function consumes
;; a name and a list of names. It determines whether any of the names
;; on the latter are equal to or an extension of the former.

; ormap
; String [List-of String] -> Boolean
(check-expect (find-name "kiran" (list "ushakiran" "azusa" "akari")) #true)
(check-expect (find-name "oribe" (list "ushakiran" "azusa oribe" "akari")) #true)
(check-expect (find-name "takashi" (list "ushakiran" "azusa oribe" "akari")) #false)
(define (find-name name lon)
  (ormap (lambda (n) (string-contains? name n)) lon))


; andmap
; String [List-of String] -> Boolean
; X [List-of X] -> Boolean
(check-expect (start-with "k" (list "ushakiran" "azusa" "akari")) #false)
(check-expect (start-with "a" (list "amma" "azusa oribe" "akari")) #true)
(define (start-with l ls)
  (andmap (lambda (s) (string=? (string-ith s 0) l)) ls))

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 15.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Inventory -> List-o-strings
; extracts the names of toys from an inventory
(define (names i)
  (cond
    [(empty? i) '()]
    [else (cons
           (IR-name (first i))
           (names (rest i)))]))

(define-struct IR
  [name price])
; An IR is a structure:
; (make-IR String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)



(define (map1 k g)
  (cond
    [(empty? k) '()]
    [else
     (cons
       (g (first k))
       (map1 (rest k) g))]))

; possible signatures with map1

; List-of-numbers [Number = Number] -> List-of-numbers
; Inventory [IR = String] -> List-of-strings


; Number -> [List-of Number]
; tabulates sin between n
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

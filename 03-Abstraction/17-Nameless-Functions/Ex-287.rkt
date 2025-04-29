#lang htdp/isl+
(require racket/string)
(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of:
; – '()
; – (cons IR Inventory)

; Example
(define in1
  (list (make-IR "ball" 40)
        (make-IR "baseball" 45)
        (make-IR "football" 50)
        (make-IR "apple" 20)
        (make-IR "tomato" 10)))

(define str-list1 (list "ball" "elephant" "mango" "banana" "cow"))
(define str-list2 (list "tomato" "bus" "aeroplane" "cow" "peacock" "elephant"))

; Number [List-of IR] -> [List-of IR]
(check-expect (eliminate-expensive 15 in1) (list (make-IR "tomato" 10)))
(define (eliminate-expensive ua in)
  (filter (lambda (ir) (< (IR-price ir) ua)) in))

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s) (contains? s (rest l)))]))

; String [List-of IR] -> [List-of IR]
; produces a list of inventory records that do not use the name ty
(check-expect (recall "ball" in1) (list (make-IR "apple" 20) (make-IR "tomato" 10)))
(define (recall str in)
  (filter (lambda (ir) (not (string-contains? (IR-name ir) str))) in))

; [List-of String] [List-of String] -> [List-of String]
; selects all those from the second one that are also on the first
(check-expect (selection str-list1 str-list2)
              (list "cow" "elephant"))
(define (selection ls1 ls2)
  (filter (lambda (str) (member str ls1)) ls2))

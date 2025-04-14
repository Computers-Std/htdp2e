#reader
(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname Ex-269) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require racket/string)

;; Exercise 269. Define eliminate-expensive. The function consumes a
;; number, ua, and a list of inventory records, and it produces a list
;; of all those structures whose sales price is below ua.

;; Then use filter to define recall, which consumes the name of an
;; inventory item, called ty, and a list of inventory records and
;; which produces a list of inventory records that do not use the name
;; ty.

;; In addition, define selection, which consumes two lists of names
;; and selects all those from the second one that are also on the
;; first.

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

(define str-list1
  (list "ball" "elephant" "mango" "banana" "cow"))
(define str-list2
  (list "tomato" "bus" "aeroplane" "cow" "peacock" "elephant"))

; Number [List-of IR] -> [List-of IR]
(check-expect (eliminate-expensive 15 in1) (list (make-IR "tomato" 10)))
(define (eliminate-expensive ua in)
  (local ; Number IR -> Boolean
      ; whether the IR-price is below UA
      ((define (isBelow? ir)
         (< (IR-price ir) ua)))
    (filter isBelow? in)))

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
  (local ; String IR -> Boolean
      ; does IR-name use String
      ((define (occur? ir)
         (not (string-contains? (IR-name ir) str))))
    (filter occur? in)))

; [List-of String] [List-of String] -> [List-of String]
; selects all those from the second one that are also on the first
(check-expect (selection str-list1 str-list2)
              (list "cow" "elephant"))
(define (selection ls1 ls2)
  (local (; String [List-of String] -> Boolean
          (define (isMember? str)
            (member? str ls1)))
    (filter isMember? ls2)))

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-268) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 268. An inventory record specifies the name of an item, a
;; description, the acquisition price, and the recommended sales
;; price.

;; Define a function that sorts a list of inventory records by the
;; difference between the two prices.

(define-struct IR
  [name description acquisition recommended])
; An IR is a structure:
;   (make-IR String String Number Number)
; An Inventory is one of:
; – '()
; – (cons IR Inventory)

(define inventory1 (list (make-IR "ball" "toy" 100 90)
                         (make-IR "apple" "fruit" 50 30)
                         (make-IR "tomato" "veggie" 30 15)))

; Inventory -> Inventory
; [List-of IR] -> [List-of IR]
; that sorts a list of inventory records by the difference between the
; two prices
(check-expect (fair-inventory '()) '())
(check-expect (fair-inventory inventory1)
              (list (make-IR "ball" "toy" 100 90)
                    (make-IR "tomato" "veggie" 30 15)
                    (make-IR "apple" "fruit" 50 30)))
(define (fair-inventory in)
  (local (; IR -> Number
          ; produce the differences b/w prices
          (define (diff-price ir)
            (- (IR-acquisition ir) (IR-recommended ir)))
          ; IR IR -> Boolean
          ; compares two IRs price-diffs
          (define (compare-2 ir1 ir2)
            (< (diff-price ir1) (diff-price ir2))))
    (sort in compare-2)))

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-261) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define-struct IR
  [name price])
; An IR is a structure:
; (make-IR String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)

;; Constants

(define inven1 (list (make-IR "apple" 2)
                     (make-IR "banana" 3)
                     (make-IR "mango" 4)
                     (make-IR "sapota" 1)))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (cond
            [(<= (IR-price (first an-inv)) 1.0)
             (cons (first an-inv) (extract1 (rest an-inv)))]
            [else (extract1 (rest an-inv))])]))

(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (local (
                  (define from-rest (extract2 (rest an-inv))))
            (cond
              [(<= (IR-price (first an-inv)) 1.0)
               (cons (first an-inv) from-rest)]
              [else from-rest]))]))

; testing
;; (extract1 inven1)
;; (extract2 inven1)

; Answer
; Both functions in terms of computation have similar performances as
; In the first function the all steps are carried by the main function
; In the second function 10% of the computation by the main and
; remaining with the local-function.
; Even though the part (extract1 (rest an-inv)), used twice, the usage
; will be only once.

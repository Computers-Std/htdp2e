#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fig-101) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define-struct IR
  [name price])
; An IR is a structure:
; (make-IR String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (cond
            [(<= (IR-price (first an-inv)) 1.0)
             (cons (first an-inv (extract1 (rest an-inv))))]
            [else (extract1 (rest an-inv))])]))

(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (local (
                  (define from-rest (extract2 (rest an-inv))))
            (cond
              [(<= (IR-price (first an-inv)) 1.0)
               (cons (first an-inv from-rest))]
              [else from-rest]))]))

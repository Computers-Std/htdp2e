#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-252) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; in book signature
; [List-of Number] (Number -> Number) -> [List-of Number]
; [List-of IR] (IR -> String) -> [List-of String]

;;; x,y :: list-of-x :: function arg :: base case => y
;;; [X Y] [List-of X] [X Y -> Y] Y -> Y
;; product:
; [Number Number] [List-of Number] (Number Number -> Number) Number -> Number
;; image*
; [Posn Image] [List-of Posn] (Posn Image -> Image) Image -> Image

; [List-of ITEM] Operation ITEM -> ITEM* (modified ITEM)
(define (fold2 l opr lcase)
  (cond
    [(empty? l) lcase]
    [else (opr (first l)
               (fold2 (rest l) opr lcase))]))

; Graphical Constants
(define emt (empty-scene 100 100))
(define dot (circle 2 "solid" "red"))
(define lop
  (list (make-posn 2 3) (make-posn 4 5) (make-posn 3 6)))

(check-expect (image* lop) (og-image* lop))
; [List-of Posn] -> Image
(define (image* l)
  (fold2 l place-dot emt))

; Posn Image -> Image
(define (place-dot p img)
  (place-image dot (posn-x p) (posn-y p) img))

; [List-of Number] -> Number
(check-expect (product (list 1 2 3)) 6)
(define (product l)
  (fold2 l * 1))

; [List-of Posn] -> Image
(define (og-image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))


; The comparison with the one to the preceding exercise yields
; interesting insight that both are exactly same functions & we can
; abstract a whole lot of functions with well written Abstraction

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-272) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
;; Exercise 272. Recall that the append function in ISL concatenates
;; the items of two lists or, equivalently, replaces '() at the end of
;; the first list with the second list:

;; (equal? (append (list 1 2 3) (list 4 5 6 7 8))
;;         (list 1 2 3 4 5 6 7 8))

;; Use foldr to define append-from-fold. What happens if you replace
;; foldr with foldl? Now use one of the fold functions to define
;; functions that compute the sum and the product, respectively, of a
;; list of numbers.

;; With one of the fold functions, you can define a function that
;; horizontally composes a list of Images. Hints (1) Look up beside
;; and empty-image. Can you use the other fold function? Also define a
;; function that stacks a list of images vertically. (2) Check for
;; above in the teac=hoacks.

; [List-of Number] [List-of Number] -> [List-of Number]
(define (append-from-foldr l1 l2)
  (foldr cons l2 l1))

; [List-of Number] [List-of Number] -> [List-of Number]
(define (append-from-foldl l1 l2)
  (foldl cons l2 (reverse l1)))

;; (equal? (append (list 1 2 3) (list 4 5 6 7 8))
;;         (append-from-foldr (list 1 2 3) (list 4 5 6 7 8)))
;; (equal? (append (list 1 2 3) (list 4 5 6 7 8))
;;         (append-from-foldl (list 1 2 3) (list 4 5 6 7 8)))

; [List-of Number] -> Number
(check-expect (sum-from-fold (list 1 2 3 4)) 10)
(define (sum-from-fold ln)
  (foldr + 0 ln))

; [List-of Number] -> Number
(check-expect (product-from-fold (list 1 2 3 4)) 24)
(define (product-from-fold ln)
  (foldr * 1 ln))

;; [List-of Image] -> Image
;; Composes a list of images horizontally.
(check-expect (img-horizontal (list (circle 5 "solid" "red") (circle 10 "solid" "blue")))
              (beside (circle 5 "solid" "red") (circle 10 "solid" "blue")))
(define (img-horizontal loi)
  (foldr beside empty-image loi))

;; [List-of Image] -> Image
;; Composes a list of images horizontally.
(check-expect (img-horizontal (list (circle 5 "solid" "red") (circle 10 "solid" "blue")))
              (above (circle 5 "solid" "red") (circle 10 "solid" "blue")))
(define (img-vertical loi)
  (foldr above empty-image loi))

; [List-of Image] Operation -> Image
; [List-of X] [X -> X] -> X
(define (compose-images loc loi)
  (foldr loc empty-image loi))

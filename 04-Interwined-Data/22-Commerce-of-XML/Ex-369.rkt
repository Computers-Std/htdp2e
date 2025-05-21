#lang htdp/isl+

;; Exercise 369. Design find-attr. The function consumes a list of
;; attributes and a symbol. If the attributes list associates the
;; symbol with a string, the function retrieves this string; otherwise
;; it returns #false. Look up assq and use it to define the function.

; An Xexpr.v2 is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '((initial "X")))
(define a1 '((afor "Apple") (bfor "Ball") (cfor "Carrot")))
(define a2 '(()))

; [List-of Attribute] Symbol -> [Maybe String]
; produce the associated string with the symbol in loa
(check-expect (find-attr a2 'afor) #false)
(check-expect (find-attr a1 'afor) "Apple")
(define (find-attr loa s)
  (if (empty? (rest loa))
      #false
      (local ((define attr (assq s loa)))
        (cond
          [(boolean? attr) attr]
          [else (second attr)]))))

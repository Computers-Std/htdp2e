#lang htdp/isl+

;; Design sexp=?, a function that determines whether two S-expressions
;; are equal.

; An S-expr (S-expression) is one of:
; - Atom
; - [List-of S-epxr]
;
; An Atom is one of:
; - Number
; - String
; - Symbol

; S-expr S-expr -> Boolean
(check-expect (sexp=? '(hello 20 "ushakiran")
                      '(hello ("twenty" 20) "ushakiran")) #false)
(check-expect (sexp=? '(hello 20 "ushakiran")
                      '(hello 20 "ushakiran")) #true)
(define (sexp=? e1 e2)
  (cond
    [(and (number? e1) (number? e2)) (= e1 e2)]
    [(and (string? e1) (string? e2)) (string=? e1 e2)]
    [(and (symbol? e1) (symbol? e2)) (symbol=? e1 e2)]
    [(and (list? e1) (list? e2))
     (cond
       [(and (empty? e1) (empty? e2)) #true]
       [(or (empty? e1) (empty? e2)) #false]
       [else (and (sexp=? (first e1) (first e2))
                  (sexp=? (rest e1) (rest e2)))])]
    [else #false]))

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-250) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 250. Design tabulate, which is the abstraction of the two
;; functions in figure 92. When tabulate is properly designed, use it
;; to define a tabulation function for sqr and tan.

; Number Operation -> [List-of Number]
; tabulates OPERATION between n and 0 (incl.) in a list
(define (tabulate n op)
  (cond
    [(= n 0) (list (op 0))]
    [else (cons (op n)
                (tabulate (sub1 n) op))]))

; Number -> [List-of Number]
; tabulates sin b/w n and 0 (incl.) in a list
(define (tab-sin n)
  (tabulate n sin))

; Number -> [List-of Number]
; tabulates sqrt b/w n and 0 (incl.) in a list
(define (tab-sqrt n)
  (tabulate n sqrt))

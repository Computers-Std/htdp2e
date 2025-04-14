#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fig-100) (read-case-sensitive #t)(teachpacks ())(htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (< (first l) (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
            (if (< (first l) smallest-in-rest)
                (first l)
                smallest-in-rest))]))

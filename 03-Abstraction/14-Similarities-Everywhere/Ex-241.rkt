#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-241) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; An NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation: non-empty lists of Celsius temperatures

; An NEList-of-booleans is one of:
; - (cons Booleans '())
; - (cons Boolean NEList-of-booleans)
; interpretation: non-empty lists of Booleans

;; Abstraction

; An [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])

(define nelot (cons 20 (cons 40 '())))
(define nelob (cons #true '()))

#reader
(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname Ex-240) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define-struct layer [stuff])

; An LStr is one of:
; - String
; - (make-layer LStr)
;; Examples
(define LSTR1 (make-layer "world"))
(define LSTR2 (make-layer LSTR1))

; An LNum is one of:
; - Number
; - (make-layer LNum)
;; Examples
(define LNUM1 (make-layer 10))
(define LNUM2 (make-layer LNUM1))

;; Abstraction
; A [Layer-of ITEM] is one of:
; - ITEM
; - (make-layer ITEM)

; A LStr is a (Layer-of String)
; A LNum is a (Layer-of Number)

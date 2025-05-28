#lang htdp/isl+

(define-struct with [left info right])
(define-struct binary [left right])

; An LOD is one of:
; - '()
; - (cons Direction LOD)

; A Direction is one of:
; - 'left
; - 'right

; A TID is one of:
; - Symbol
; - (make-binary TID TID)
; - (make-with TID Symbol TID)

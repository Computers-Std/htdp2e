#lang htdp/isl+

(define-struct dir [name content size permis])
; A Dir.v2 is a structure
; (make-dir String LOFD Number Boolean)

; An LOFD (short for List of Files and Directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

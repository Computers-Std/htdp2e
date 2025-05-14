#lang htdp/isl+

(define-struct dir [name content])

; A Dir.v2 is a structure
; (make-dir String LOFD)

; An LOFD (short for List of Files and Directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

;; (define Dir-TS (make-dir "TS" ))

(define Dir-Code (make-dir "Code" (list "hang" "draw")))
(define Dir-Docs (make-dir "Docs" (list "read!")))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs)))
(define Dir-Text (make-dir "Text" (list "part1" "part2" "part3")))
(define Dir-TS (make-dir "TS" (list Dir-Text "read!" Dir-Libs)))

#lang htdp/isl+

(define-struct file [name size content])
; A File.v3 is a structure:
; (make-file String N Stringe)

(define-struct dir [name dirs files])
; A Dir.v3 is a structure:
; (make-dir String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

(define Dir-Code (make-dir "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 ""))))
(define Dir-Docs (make-dir "Docs" '() (list (make-file "read!" 19 ""))))
(define Dir-Text
  (make-dir "Text"
            '()
            (list (make-file "part1" 99 "") (make-file "part2" 52 "") (make-file "part3" 17 ""))))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs) '()))
(define Dir-TS (make-dir "TS" (list Dir-Text Dir-Libs) (list (make-file "read!" 10 ""))))

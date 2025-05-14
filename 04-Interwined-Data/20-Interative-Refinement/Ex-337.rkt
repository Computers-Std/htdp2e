#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct file [name size content])
; A File.v3 is a structure:
; (make-file String N Stringe)

(define-struct dir [name lod lof])
; A Dir.v3 is a structure:
; (make-dir String [List-of Dir.v3] [List-of File.v3])

(define Dir-Code (make-dir "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 ""))))
(define Dir-Docs (make-dir "Docs" '() (list (make-file "read!" 19 ""))))
(define Dir-Text
  (make-dir "Text"
            '()
            (list (make-file "part1" 99 "") (make-file "part2" 52 "") (make-file "part3" 17 ""))))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs) '()))
(define Dir-TS (make-dir "TS" (list Dir-Text Dir-Libs) (list (make-file "read!" 10 ""))))

; Dir -> Number
(check-expect (how-many Dir-Text) 3)
(check-expect (how-many Dir-Libs) 3)
(check-expect (how-many Dir-TS) 7)

(define (how-many dir)
  (foldr (lambda (d sum) (+ (how-many d) sum))
         (length (dir-lof dir)) (dir-lod dir)))

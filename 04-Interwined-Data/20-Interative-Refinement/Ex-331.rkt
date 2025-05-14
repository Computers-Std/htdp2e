#lang htdp/isl+

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String

(define Dir-Text '("part1" "part2" "part3"))
(define Dir-Code '("hang" "draw"))
(define Dir-Docs '("read!"))
(define Dir-Libs (list Dir-Code Dir-Docs))
(define Dir-TS (list Dir-Text "read!" Dir-Libs))

; Dir -> Number
; counts number of Files in a Dir
(check-expect (how-many Dir-Text) 3)
(check-expect (how-many Dir-Libs) 3)
(check-expect (how-many Dir-TS) 7)
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else (+ (if (string? (first dir))
                 1 (how-many (first dir)))
             (how-many (rest dir)))]))

#lang htdp/isl+

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String

(define Dir-Text '("part1" "part2" "part3"))
(define Dir-Code '("hang" "draw"))
(define Dir-Docs '("read!"))
;; (define Dir-Libs '(Dir-Code Dir-Docs))
;; (define Dir-TS '(Dir-Text "read!" Dir-Libs))

;; (define Dir-Text (list "part1" "part2" "part3"))
;; (define Dir-Code (list "hang" "draw"))
;; (define Dir-Docs (list "read!"))
(define Dir-Libs (list Dir-Code Dir-Docs))
(define Dir-TS (list Dir-Text "read!" Dir-Libs))

(check-expect Dir-TS '(("part1" "part2" "part3")
                       "read!"
                       (("hang" "draw")
                        ("read!"))))

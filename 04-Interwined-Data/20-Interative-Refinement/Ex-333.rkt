#lang htdp/isl+

(define-struct dir [name content])
; A Dir.v2 is a structure
; (make-dir String LOFD)

; An LOFD (short for List of Files and Directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

(define Dir-Code (make-dir "Code" (list "hang" "draw")))
(define Dir-Docs (make-dir "Docs" (list "read!")))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs)))
(define Dir-Text (make-dir "Text" (list "part1" "part2" "part3")))
(define Dir-TS (make-dir "TS" (list Dir-Text "read!" Dir-Libs)))

; Dir -> Number
; counts number of Files in a Dir
(check-expect (how-many Dir-Text) 3)
(check-expect (how-many Dir-Libs) 3)
(check-expect (how-many Dir-TS) 7)
(define (how-many dir)
  (local ((define (for-lofd lofd)
            (cond
              [(empty? lofd) 0]
              [else (+ (if (dir? (first lofd))
                           (for-lofd (dir-content (first lofd)))
                           1)
                       (for-lofd (rest lofd)))])))
    (for-lofd (dir-content dir))))

;;; Compact but Ugly solution
(check-expect (how-many-ugly Dir-Text) 3)
(check-expect (how-many-ugly Dir-Libs) 3)
(check-expect (how-many-ugly Dir-TS) 7)
(define (how-many-ugly dr)
  (cond
    [(empty? (dir-content dr)) 0]
    [else
     (+ (if (string? (first (dir-content dr)))
            1 (how-many-ugly (first (dir-content dr))))
        (how-many-ugly (make-dir "@" (rest (dir-content dr)))))]))

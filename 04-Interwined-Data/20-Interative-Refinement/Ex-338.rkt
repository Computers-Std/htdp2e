#lang htdp/isl+
(require htdp/dir)

(define L (create-dir "/home/ukiran/htdp/"))

; Dir -> Number
; counts no. of Files in a Dir
(define (how-many dir)
  (foldr (lambda (d sum) (+ (how-many d) sum))
         (length (dir-files dir)) (dir-dirs dir)))

(check-expect (how-many L) 7)

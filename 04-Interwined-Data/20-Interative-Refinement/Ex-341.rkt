#lang htdp/isl+
(require htdp/dir)
(require 2htdp/abstraction)

(define L (create-dir "/home/ukiran/prog/htdp2e/04-Interwined-Data/20-Interative-Refinement/testdir"))
(define pwd (create-dir "/home/ukiran/prog/htdp2e/04-Interwined-Data/20-Interative-Refinement/"))

; Dir -> Number
; produce the size of entire directory tree
(define (mdu d)
  (local
      (; [List-of File] -> Number
       (define (du-files lf)
         (foldr (lambda (fi total) (+ (file-size fi)
                                      total)) 0 lf))
       ; [List-of Dir] -> Number
       (define (du-dirs ld)
         (cond
           [(empty? ld) 0]
           [else (add1 (+ (du-files (dir-files (first ld)))
                          (du-dirs (dir-dirs (first ld)))
                          (du-dirs (rest ld))))])))
    (+ (du-files (dir-files d))
       (du-dirs (dir-dirs d)))))

(define (du d)
  (+ (for/sum ([f (dir-files d)]) (file-size f))
     (for/sum ([d (dir-dirs d)]) (add1 (du d)))))

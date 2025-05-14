#lang htdp/isl+
(require htdp/dir)

(define L (create-dir "/home/ukiran/htdp/"))

; Dir String -> Boolean
; determine whether File(name) is present or not.
(check-expect (find? L "file1.txt") #true)
(check-expect (find? L "file7.txt") #true)
(check-expect (find? L "file8.txt") #false)
(define (find? d f)
  (local (; [List-of File] -> Boolean
          (define (find-in-files? lf)
            (cond
              [(empty? lf) #false]
              [else (ormap (lambda (fi) (string=? f (file-name fi)))
                           lf)]))
          ; [List-of Dir] -> Boolean
          (define (find-in-dirs? ld)
            (cond
              [(empty? ld) #false]
              [else (or (find-in-files? (dir-files (first ld)))
                        (find-in-dirs? (dir-dirs (first ld)))
                        (find-in-dirs? (rest ld)))])))
    (or (find-in-files? (dir-files d))
        (find-in-dirs? (dir-dirs d)))))

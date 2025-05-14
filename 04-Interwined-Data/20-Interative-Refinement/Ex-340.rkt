#lang htdp/isl+
(require htdp/dir)

(define test
  (create-dir "/home/ukiran/prog/htdp2e/04-Interwined-Data/20-Interative-Refinement/testdir/"))
(define pwd (create-dir "/home/ukiran/prog/htdp2e/04-Interwined-Data/20-Interative-Refinement"))

;; ; Dir -> [List-of String]
;; ; lists the names of all files and directories in a given Dir
(define (ls dr)
  (local ; [List-of File] -> [List-of String]
      ((define (ls-files lf)
         (map (lambda (fi) (file-name fi)) lf))
       ; [List-of Dir] -> [List-of String]
       (define (ls-dirs ld)
         (foldr (lambda (di lodn) (cons (dir-name di) lodn)) '() ld)))
    (append (ls-dirs (dir-dirs dr)) (ls-files (dir-files dr)))))

; With Hierarchy
(define (lsh dr)
  (local ((define (ls-files lf)
            (map (lambda (fi) (file-name fi)) lf))
          (define (dir->names d)
            (cons (dir-name d) (map dir->names (dir-dirs d)))))
    (append (ls-files (dir-files dr)) (map dir->names (dir-dirs dr)))))

(check-expect (lsh pwd)
              (list "Ex-329.rkt"
                    "Ex-330.rkt"
                    "Ex-331.rkt"
                    "Ex-332.rkt"
                    "Ex-333.rkt"
                    "Ex-334.rkt"
                    "Ex-335.rkt"
                    "Ex-336.rkt"
                    "Ex-337.rkt"
                    "Ex-338.rkt"
                    "Ex-339.rkt"
                    "Ex-340.rkt"
                    "Ex-341.rkt"
                    "Ex-342.rkt"
                    "Ex-343.rkt"
                    "re-342.rkt"
                    (list "testdir"
                          (list "dir1")
                          (list "dir2" (list "dir5"))
                          (list "dir3" (list "dir6") (list "dir7"))
                          (list "dir4"))))

;; (ls test)
;; (ls pwd)

;; (lsh test)
;; (lsh pwd)

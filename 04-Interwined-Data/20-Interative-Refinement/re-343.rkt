#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct file [name size content])
;; A File is a structure:
;;   (make-file String N String)

(define-struct dir [name dirs files])
;; A Dir is a structure:
;;    (make-dir String [List-of Dir] [List-of File])

; A Path is [List-of String].
; interpretation directions into a directory tree

(define Dir-Text
  (make-dir "Text"
            '()
            (list (make-file "part1" 99 "") (make-file "part2" 52 "") (make-file "part3" 17 ""))))
(define Dir-Code (make-dir "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 ""))))
(define Dir-Docs (make-dir "Docs" '() (list (make-file "read!" 19 ""))))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs) '()))
(define Dir-TS (make-dir "TS" (list Dir-Text Dir-Libs) (list (make-file "read!" 10 ""))))
(define Dir-TS2
  (make-dir "TS" (list Dir-Text Dir-Libs) (list (make-file "read!" 10 "") (make-file "read!" 19 ""))))
(define Dir-Empty (make-dir "Empty" '() '()))

;; Exercise 343. Design the function ls-R, which lists the paths to
;; all files contained in a given Dir

;; from 'Yugaego'
(define (ls-R-c1 d)
  (local ;; [List-of File] -> [List-of Path]
      ((define (list-files lof)
         (map (lambda (f) (append (list (dir-name d) (file-name f)))) lof))
       ;; [List-of Dir] -> [List-of Path]
       (define (list-dirs lod)
         (cond
           [(empty? lod) '()]
           [else
            (append (map (lambda (l) (cons (dir-name d) l)) (ls-R-c1 (first lod)))
                    (list-dirs (rest lod)))])))
    (append (list-files (dir-files d)) (list-dirs (dir-dirs d)))))
; c1 ans:
;; (list (list "TS" "read!")
;;       (list "TS" "read!")
;;       (list "TS" "Text" "part1")
;;       (list "TS" "Text" "part2")
;;       (list "TS" "Text" "part3")
;;       (list "TS" "Libs" "Code" "hang")
;;       (list "TS" "Libs" "Code" "draw")
;;       (list "TS" "Libs" "Docs" "read!"))

;; from 'Bgusach'
(define (ls-R-c2 dir)
  (local ((define (ls dir)
            (append (for/list ([d (dir-dirs dir)]) (dir-name d))
                    (for/list ([f (dir-files dir)]) (file-name f))))
          (define local-files (map list (ls dir)))
          (define subdir-files
            (for*/list ([subdir (dir-dirs dir)]
                        [subpath (ls-R-c2 subdir)])
              (cons (dir-name subdir) subpath))))
    (append local-files subdir-files)))
; c2 ans:
;; (list (list "Text")
;;       (list "Libs")
;;       (list "read!")
;;       (list "read!")
;;       (list "Text" "part1")
;;       (list "Text" "part2")
;;       (list "Text" "part3")
;;       (list "Libs" "Code")
;;       (list "Libs" "Docs")
;;       (list "Libs" "Code" "hang")
;;       (list "Libs" "Code" "draw")
;;       (list "Libs" "Docs" "read!"))

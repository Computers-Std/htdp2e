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
(define Dir-Test (make-dir "Test" (list Dir-Empty) (dir-files Dir-Text)))


; Dir String -> [Maybe [List-of Path]]
; this finds duplicate file-paths to.
(check-expect (find-all Dir-TS2 "read!")
              (list (list "read!") (list "read!") (list "Libs" "Docs" "read!")))
(define (find-all dir name)
  (local ((define (isfile? f)
            (string=? (file-name f) name))
          (define file-here? (ormap (lambda (fi) (isfile? fi)) (dir-files dir)))
          (define find-in-subdirs
            (for*/list ([subdir (dir-dirs dir)]
                        [path (find-all subdir name)])
              (cons (dir-name subdir) path))))
    (if file-here?
        (append
         (map (lambda (fi) (list (file-name fi)))
              (filter isfile? (dir-files dir)))
         find-in-subdirs)
        find-in-subdirs)))

; Dir -> [List-of Path]
; lists the paths to all files in Dir
(define (ls-R dir)
  (local ; [List-of File] -> [List-of Path]
      ((define ls-r-files
         (append (map (lambda (fi) (list (file-name fi))) (dir-files dir))
                 (map (lambda (di) (list (dir-name di))) (dir-dirs dir))))
       ; [List-of Dir] -> [List-of Path]
       (define ls-r-subdirs
         (for*/list ([subdir (dir-dirs dir)]
                     [path (ls-R subdir)])
           (cons (dir-name subdir) path))))
    (append ls-r-files ls-r-subdirs)))

(check-expect (ls-R Dir-TS2)
              (list (list "read!")
                    (list "read!")
                    (list "Text")
                    (list "Libs")
                    (list "Text" "part1")
                    (list "Text" "part2")
                    (list "Text" "part3")
                    (list "Libs" "Code")
                    (list "Libs" "Docs")
                    (list "Libs" "Code" "hang")
                    (list "Libs" "Code" "draw")
                    (list "Libs" "Docs" "read!")))

(check-expect (find-all-2 Dir-TS2 "read!")
              (find-all Dir-TS2 "read!"))
(check-expect (find-all-2 Dir-Test "Empty") (list (list "Empty")))

; Dir String -> [Maybe [List-of Path]]
(define (find-all-2 dir name)
  (filter (lambda (path)
            (member? name path)) (ls-R dir)))

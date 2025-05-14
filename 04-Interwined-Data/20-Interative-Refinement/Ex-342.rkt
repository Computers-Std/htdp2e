#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct file [name size content])
;; A File is a structure:
;;   (make-file String N String)

(define-struct dir [name dirs files])
;; A Dir is a structure:
;;    (make-dir String [List-of Dir] [List-of File])

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

; Dir String -> [Maybe [List-of String]]
(define (find dir name)
  (local ; Check in dir-files of Dir
      ((define file-here? (ormap (lambda (f) (string=? (file-name f) name)) (dir-files dir))))
    ; -- IN --
    (if file-here?
        (list name)
        (for/or ([subdir (dir-dirs dir)])
          (local ; Dir String -> [Maybe [List-of String]]
              ; Traverse through all subdirs of each 'Subdir'
              ((define traverse (find subdir name)))
            ; -- IN --
            (if (false? traverse)
                #false
                (cons (dir-name subdir) traverse)))))))

(define (find-2 dir name)
  (local ((define file-here? (ormap (lambda (fi) (string=? (file-name fi) name)) (dir-files dir))))
    (if file-here?
        (list name)
        ((lambda (subdir)
           (local ((define traverse (find-2 subdir name)))
             (if (false? traverse)
                 #false
                 (cons (dir-name subdir) traverse))))
         (dir-dirs dir)))))

; Dir String -> [Maybe [List-of Path]]
(define (find-all dir name)
  (local ((define file-here? (ormap (lambda (fi) (string=? (file-name fi) name)) (dir-files dir)))
          ; [List-of Dir] -> [List-of Path]
          (define find-in-subdirs
            (for*/list ([subdir (dir-dirs dir)]
                        [path (find-all subdir name)])
              (cons (dir-name subdir) path))))
    (if file-here?
        (cons (list name) find-in-subdirs)
        find-in-subdirs)))

; Dir String -> [Maybe [List-of Path]]
; this finds duplicate file-paths to.
(check-expect (find-all-2 Dir-TS2 "read!")
              (list (list "read!") (list "read!") (list "Libs" "Docs" "read!")))
(define (find-all-2 dir name)
  (local ((define (isfile? f)
            (string=? (file-name f) name))
          (define file-here? (ormap (lambda (fi) (isfile? fi)) (dir-files dir)))
          (define find-in-subdirs
            (for*/list ([subdir (dir-dirs dir)]
                        [path (find-all-2 subdir name)])
              (cons (dir-name subdir) path))))
    (if file-here?
        (append
         (map (lambda (fi) (list (file-name fi)))
              (filter isfile? (dir-files dir)))
         find-in-subdirs)
        find-in-subdirs)))

;; #[/home/ukiran/prog/htdp2e/resrc/bgusach-htdp2e/4-intertwined-data/ex-338-to-344.rkt:L103]
;; [13-05-2025] NOTE: This is the most beautiful piece of code I've
;; seen so far that I can understand

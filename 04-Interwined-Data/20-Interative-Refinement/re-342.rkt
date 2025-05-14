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
(define Dir-Empty (make-dir "Empty" '() '()))

; Dir String -> [Maybe [List-of String]]
(define (find dir name)
  (local ((define file-here?
            (ormap (lambda (fi) (string=? (file-name fi) name)) (dir-files dir))))
    (if file-here?
        (list name)
        (for/or ([subdir (dir-dirs dir)])
          (if (false? (find subdir name))
              #false
              ;; Note the Name of Subdir and Go inside the subdir
              (cons (dir-name subdir) (find subdir name)))))))

; Dir String -> [Maybe [List-of String]]
(define (find-2 dir name)
  (local ((define file-here?
            (ormap (lambda (fi) (string=? (file-name fi) name)) (dir-files dir))))
    (if file-here?
        (list name)
        ((lambda (subdir)
           (local ((define traverse (find-2 subdir name)))
             (if (false? traverse)
                 #false
                 (cons (dir-name subdir) traverse))))
         (dir-dirs dir)))))

; [List-of Number] -> [List-of Number]
(define (test-for/list ls)
  (for/list ([i (sub1 (length ls))]
             [j ls])
    (list i j)))

;; (define (test-for*/list ls)
;;   (for*/list ([i (sub1 (length ls))] [j (+ i (first ls))])
;;     (list i j)))

(define (test-for*/list ls)
  (for*/list ([i (length ls)] [j i])
    (list i j)))

; [List-of Number] Number -> [List-of Number]
; produce a list of Before and After of given Number

; --

(define (file-here? dir name)
  (ormap (lambda (fi) (string=? (file-name fi) name)) (dir-files dir)))

; Dir String -> [Maybe [List-of Path]]
(define (find-all dir name)
  (local ((define find-in-subdirs
            (for*/list ([subdir (dir-dirs dir)]
                        [path (find-all subdir name)])
              (cons (dir-name subdir) path))))
    (if (file-here? dir name)
        (cons (list name) find-in-subdirs)
        find-in-subdirs)))

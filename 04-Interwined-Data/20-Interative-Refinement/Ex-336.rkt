#lang htdp/isl+

(define-struct file [name size content])
; A File.v3 is a structure:
; (make-file String N Stringe)

(define-struct dir [name sdirs files])
; A Dir.v3 is a structure:
; (make-dir String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

(define Dir-Code (make-dir "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 ""))))
(define Dir-Docs (make-dir "Docs" '() (list (make-file "read!" 19 ""))))
(define Dir-Text
  (make-dir "Text"
            '()
            (list (make-file "part1" 99 "") (make-file "part2" 52 "") (make-file "part3" 17 ""))))
(define Dir-Libs (make-dir "Libs" (list Dir-Code Dir-Docs) '()))

(define Dir-TS (make-dir "TS" (list Dir-Text Dir-Libs) (list (make-file "read!" 10 ""))))

; Dir -> Number
; counts no. of Files in a Dir
(check-expect (how-many Dir-Text) 3)
(check-expect (how-many Dir-Libs) 3)
(check-expect (how-many Dir-TS) 7)
(define (how-many dir)
  (local (; [List-of File] -> Number
          (define (in-files lof)
            (length lof))
          ; [List-of Dir] -> Number
          (define (in-dirs lod)
            (cond
              [(empty? lod) 0]
              [else (+ (in-files (dir-files (first lod)))
                       (in-dirs (dir-sdirs (first lod)))
                       (in-dirs (rest lod)))])))
    (+ (in-files (dir-files dir))
       (in-dirs (dir-sdirs dir)))))

;; Ugly Solution
(define (how-many-ugly dr)
  (local ; [List Dir] [List File] -> Number
      ((define (for-dirs lod lof)
         (cond
           [(empty? lod) (length lof)]
           [else
            (+ (if (dir? (first lod))
                   (for-dirs (dir-sdirs (first lod)) (dir-files (first lod)))
                   1)
               (for-dirs (rest lod) lof))])))
    (for-dirs (dir-sdirs dr) (dir-files dr))))

;; Given the complexity of the data definition, contemplate how anyone
;; can design correct functions. Why are you confident that how-many
;; produces correct results?

;; As the given data definition contains three distinct fields, one of
;; which is self-referential, one can obtain the desired results by
;; acting on each respective field

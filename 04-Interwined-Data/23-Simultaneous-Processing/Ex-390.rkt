#lang htdp/isl+

;; Exercise 390. Design the function tree-pick. The function consumes
;; a tree of symbols and a list of directions:

(define-struct branch [left right])

; A TOS is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A list if Directions is also called a path.

(define ERROR "Given a Symbol, expected: TOS")
(define tos1 'a)
(define tos2 (make-branch tos1 'b))
(define tos3 (make-branch tos2 'c))
(define tos4 (make-branch tos3 'd))
(define tos5 (make-branch tos1 tos3))

; TOS [List-of Direction] -> [Maybe TOS]
(check-expect (tree-pick tos3 '(left right)) 'b)
(check-expect (tree-pick tos1 '()) tos1)
(check-error (tree-pick tos1 '(left)) ERROR)
(check-expect (tree-pick tos5 '(right left)) (make-branch 'a 'b))
(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod)) tos]
    [(and (symbol? tos) (cons? lod)) (error ERROR)]
    [(and (branch? tos) (empty? lod)) tos]
    [(and (branch? tos) (cons? lod))
     (tree-pick (if (symbol=? 'left (first lod))
                    (branch-left tos)
                    (branch-right tos))
                (rest lod))]))

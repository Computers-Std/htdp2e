#lang htdp/isl+

;; Exercise 324. Design the function inorder. It consumes a binary
;; tree and produces the sequence of all the ssn numbers in the tree
;; as they show up from left to right when looking at a tree drawing.
;; Hint: Use append.

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define t1 (make-node 15 'dave NONE (make-node 24 'i NONE NONE)))
(define t2 (make-node 15 'azusa (make-node 87 'h NONE NONE) NONE))

; BT -> [List-of Number]
; produces the sequence of all ssn numbers in tree from left to right
(check-expect (inorder t1) (list 15 24))
(check-expect (inorder t2) (list 87 15))
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt))
                  (list (node-ssn bt))
                  (inorder (node-right bt)))]))

;; What does inorder produce for a binary search ?
;; Ans:
; Given any BT with "Ordered Numbers", _inorder_ traverses from the
; Left-Most node to Right-Most node, hence collect all numbers in the
; same order as they show up.

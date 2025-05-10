#lang htdp/isl+

;; Exercise 325. Design search-bst. The function consumes a number n
;; and a BST. If the tree contains a node whose ssn field is n, the
;; function produces the value of the name field in that node.
;; Otherwise, the function produces NONE. The function organization
;; must exploit the BST invariant so that the function performs as few
;; comparisons as necessary.
;; See exercise 189 for searching in sorted lists. Compare!

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BST (short for Binary Search Tree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)
; Data Invariant:
; (node-ssn node-left) < node-ssn <(node-ssn node-right)

(define t1-l (make-node 13 'u NONE NONE))
(define t1 (make-node 15 'd t1-l (make-node 24 'i NONE NONE)))

; BST Number -> [Maybe Symbol]
; If number found in BST return Name or else return NONE
(check-expect (search-bst t1 13) 'u)
(check-expect (search-bst t1 1) NONE)
(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [(= (node-ssn bst) n) (node-name bst)]
    [else (if (> (node-ssn bst) n)
              (search-bst (node-left bst) n)
              (search-bst (node-right bst) n))]))

; In Exercise-189,
; Even I am dealing with a Ordered List, I started search from the
; first element, where as in BST, I started search form the current
; root, that means I ignore either of the two nodes(L, R).
; In Ex-189, I could start from middle element (/ len 2), that would
; yield similar performance.

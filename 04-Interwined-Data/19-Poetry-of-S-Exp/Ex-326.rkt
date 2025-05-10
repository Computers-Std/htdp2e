#lang htdp/isl+

;; Exercise 326. Design the function create-bst. It consumes a BST B,
;; a number N, and a symbol S. It produces a BST that is just like B
;; and that in place of one NONE subtree contains the node structure
;; (make-node N S NONE NONE)
;; Once the design is completed, use the function on tree A from
;; figure 119.

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

; BST Number Symbol -> BST
; Insert a new node in BST
(define (create-bst b n s)
  (cond
    [(no-info? b) (make-node n s NONE b)]
    [else
     (make-node (node-ssn b)
                (node-name b)
                (if (> (node-ssn b) n)
                    (create-bst (node-left b) n s)
                    (node-left b))
                (if (< (node-ssn b) n)
                    (create-bst (node-right b) n s)
                    (node-right b)))]))

; [2025-05-09 Fri] NOTE: I copied a part of this solution.
; I didn't fully understand the question initially.

; Explanation: First Clause is self eplanatory and Second Clause is an
; Else statement, Following the Parent (root) Node, In Left and Right
; fields only one of them will be evaluated, if Number(n) is less than
; Parent's ssn, the work must be done on the Left-side of the node.
; That is to traverse on the Left-side of node, If not keep the left
; part as it is (node-left b) and traverse in Right-side of node
; (node-right b)

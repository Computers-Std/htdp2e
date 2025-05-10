#lang htdp/isl+

;;  327. Design the function create-bst-from-list. It consumes a list
;;  of numbers and names and produces a BST by repeatedly applying
;;  create-bst. Here is the signature:

(define-struct no-info [])
(define NONE (make-no-info))
(define-struct node [ssn name left right])
; A BST (short for Binary Search Tree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)
; Data Invariant:
; (node-ssn node-left) < node-ssn <(node-ssn node-right)

; BST Number Symbol -> BST
; Insert a new node in BST

; [List-of [List Number Symbol]] -> BST
(define (create-bst-from-list lolns)
  (local ; BST Number Symbol -> BST
      ((define (create-bst b n s)
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
       ; [List Number Symbol] BST
       (define (traverse p bsts)
         (create-bst bsts (first p) (second p))))
    (cond
      [(or (empty? lolns)
           (empty? (rest lolns))) NONE]
      [else (foldr traverse NONE lolns)])))

(define tr '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))
;; (create-bst-from-list tr)

#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define t1 (make-node 15 'dave NONE (make-node 24 'i NONE NONE)))
(define t2 (make-node 15 'azusa (make-node 87 'h NONE NONE) NONE))

; Number BT -> Symbol or Boolean
; does BT contains n, if true say Name
(check-expect (search-bt t1 24) 'i)
(check-expect (search-bt t2 24) #false)
(define (search-bt bt n)
  (cond
    [(no-info? bt) #false]
    [(= (node-ssn bt) n) (node-name bt)]
    [else (if (boolean? (search-bt (node-left bt) n))
              (search-bt (node-right bt) n)
              (search-bt (node-left bt) n))]))

; [2025-05-08 Thu] NOTE: This is not my solution, I just copied.

; Explanation: Searching in BT will either produces Boolean or Symbol,
; If the result of searching in Left-Side of BT is a Symbol, then
; return that Symbol, else Search through the Right-Side of BT.

; I never encountered something like before.

; [2025-05-08 Thu] TODO: Are there any better alternatives to this.

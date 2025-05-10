#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define t1 (make-node 15 'd NONE (make-node 24 'i NONE NONE)))
(define t2 (make-node 15 'd (make-node 87 'h NONE NONE) NONE))

;; BT Number -> Boolean
;; Determines whether n occurs in the given BT.
(check-expect (contains-bt? NONE 10) #false)
(check-expect (contains-bt? t1 10) #false)
(check-expect (contains-bt? t1 15) #true)
(check-expect (contains-bt? t2 15) #true)
(check-expect (contains-bt? t2 87) #true)
(define (contains-bt? t n)
  (cond
    [(no-info? t) #false]
    [else (or (= n (node-ssn t))
              (contains-bt? (node-left t) n)
              (contains-bt? (node-right t) n))]))

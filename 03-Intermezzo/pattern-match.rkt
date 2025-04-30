#lang htdp/isl+
(require 2htdp/abstraction)

;; --- Sample Problem 1
; A [Non-empty-list X] is one of:
; – (cons X '())
; – (cons X [Non-empty-list X])

; [Non-empty-list X] -> X
; retrieves the last item of ne-l
(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))
(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))

;; --- Sample Problem 2
(define-struct layer [color doll])
; An RD.v2 (short for Rusian doll) is one of:
; - "doll"
; - (make-layer String RD.v2)

; RD.v2 -> N
; how many dolls are a part of an-rd
(check-expect (depth (make-layer "red" "doll")) 1)
(check-expect (depth (make-layer "red" (make-layer "green" "doll"))) 2)
(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer c inside) (+ (depth inside) 1)]))

;; --- Sample Problem 3: UFO

; [List-of Posn] -> [List-of Posn]
; moves each object right by delta-x pixels

(define input `(,(make-posn 1 1) ,(make-posn 10 14)))
(define output `(,(make-posn 4 1) ,(make-posn 13 14)))

(check-expect (move-right input 3) output)
(define (move-right lop delta-x)
  (for/list ([p lop])
    (match p
      [(posn x y) (make-posn (+ x delta-x) y)])))

; with cond
(check-expect (cond-move-right input 3) output)
(define (cond-move-right lop delta-x)
  (cond
    [(empty? lop) '()]
    [else (cons ((lambda (p)
                   (make-posn (+ delta-x (posn-x p))
                              (posn-y p))) (first lop))
                (cond-move-right (rest lop) delta-x))]))

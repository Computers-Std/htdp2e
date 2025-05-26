#lang htdp/isl+

; [List-of Symbol] [List-of Number] -> [List-of Pair]
; A Pair is (cons Symbol (cons Number '()))
(check-expect (cross '() '()) '())
(check-expect (cross '(a) '()) '())
(check-expect (cross '() '()) '())
(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a) '(1 2)) '((a 1) (a 2)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b) '(1 2)) '((a 1) (a 2) (b 1) (b 2)))
(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross los lon)
  (local ((define (for-sym s ln)
            (cond
              [(empty? ln) '()]
              [else (cons (list s (first ln))
                          (for-sym s (rest ln)))])))
    (cond
      [(empty? los) '()]
      [else (append (for-sym (first los) lon)
                    (cross (rest los) lon))])))

 (define (cross-abstract los lon)
   (foldr
    (lambda (s l)
      (append (map (lambda (n) (cons s (cons n '()))) lon)
              l))
    '() los))

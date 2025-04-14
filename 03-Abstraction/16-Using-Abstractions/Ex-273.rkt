#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-273) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Exercise 273. The fold functions are so powerful that you can
;; define almost any list processing functions with them. Use fold to
;; define map.

(check-expect (map-fold add1 (list 1 2 3))
              (map add1 (list 1 2 3)))

; [X -> X] [List-of X] -> [List-of X]
(define (map-fold fun lon)
  (local (
          (define (bolt x y)
            (cons (fun x) y)))
    (foldr bolt '() lon)))

; NOTE: even though I didn't (am unable to) came up with the solution
; it (bolt) is a revelation for me.

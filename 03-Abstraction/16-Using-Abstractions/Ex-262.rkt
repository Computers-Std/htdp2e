#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 262. Design the function identityM, which creates diagonal
;; squares of 0s and 1s:

;; > (identityM 1)
;; (list (list 1))
;; > (identityM 3)
;; (list (list 1 0 0) (list 0 1 0) (list 0 0 1))

;; Use the structural design recipe and exploit the power of local.

; Number -> [List-of [List-of Number]]
; interpretaion: given size of the matrix, produces the Identity
; Matrix
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (identityM size)
  (cond
    [(= size 0) '()]
    [else (local (
                  ; Number Number Number -> [List-of Number]
                  (define (make-row size pos current)
                    (cond
                      [(>= current size) '()]
                      [else (cons
                             (if (= current pos) 1 0)
                             (make-row size pos (add1 current)))]))
                  ; Number Number -> [List-of [List-of Number]]
                  (define (make-matrix size pos)
                    (cond
                      [(>= pos size) '()]
                      [else (cons (make-row size pos 0)
                                  (make-matrix size (add1 pos)))])))
            (make-matrix size 0))]))

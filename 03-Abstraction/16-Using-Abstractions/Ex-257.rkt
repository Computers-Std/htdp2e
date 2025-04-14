#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-257) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; (build-list 5 add1)

; Number (Number -> Number) -> [List-of Number]
(check-expect (build-l*st 3 add1) (list 1 2 3))
(check-expect (build-l*st 3 sqr) (list 0 1 4))
(define (build-l*st n fun)
  (cond
    [(= n 0) '()]
    [else (add-to-end (fun (sub1 n))
                      (build-l*st (sub1 n) fun))]))

; add-to-end
; X [List-of X] ->[List-of X]
; add X to the end of list
(check-expect (add-to-end 4 (list 1 2 3)) (list 1 2 3 4))
(define (add-to-end n l)
  (cond
    [(empty? l) (cons n '())]
    [else (cons (first l) (add-to-end n (rest l)))]))

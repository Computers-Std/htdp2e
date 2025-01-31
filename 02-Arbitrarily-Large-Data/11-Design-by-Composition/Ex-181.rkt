#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-181) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 181. Use list to construct the equivalent of these lists:

;; (cons "a" (cons "b" (cons "c" (cons "d" '()))))

;; (cons (cons 1 (cons 2 '())) '())

;; (cons "a" (cons (cons 1 '()) (cons #false '())))

;; (cons (cons "a" (cons 2 '())) (cons "hello" '()))

;; Also try your hand at this one:

;; (cons (cons 1 (cons 2 '()))
;;       (cons (cons 2 '())
;;             '()))

(list "a" "b" "c" "d")
(list (list 1 2))
(list "a" (list 1) (list #false))
(list (list "a" (list 2)) (list "hello"))

(list (list 1 (list 2))
      (list (list 2)))

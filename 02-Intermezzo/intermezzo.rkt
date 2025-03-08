#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname intermezzo) (read-case-sensitive #t)
                      (teachpacks ())
                      (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;;; Intermezzo 2: Quote and Unquote

;; '(1 2 3)

;; '(("a" 1)
;;   ("b" 2)
;;   ("c" 3))

;; Ex-231

;1
'(1 "a" 2 #false 3 "c")
(list 1 "a" 2 #false 3 "c")
(cons 1 (cons "a" (cons 2 (cons #false (cons 2 (cons "c" '()))))))

;2
'()
(list)
'()

;;3
'(("alan" 1000) ("barb" 2000) ("carl" 1500))
(list (list "alan" 1000) (list "barb" 2000) (list "carl" 1500))
(cons (cons "alan" (cons 1000 '()))
      (cons (cons "barb" (cons 2000 '()))
            (cons (cons "carl" (cons 1500 '())) '())))

(define x 42)
'(40 41 x 43 44)

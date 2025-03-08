#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname intermezzo-2.2) (read-case-sensitive #t)
                          (teachpacks ())
                          (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;;; Intermezzo 2: Quasiquote and Unquote

`(1 2 3)
`("a" "b" "c")

(define x 42)

`(40 41 ,x 43 44)
(quasiquote (40 41 (unquote x) 43 44))

`(1 ,(+ 1 1) 3)
(quasiquote (1 (unquote (+ 1 1)) 3))

;; Ex-232
(list 1 "a" 2 #false 3 "c")
(list (list "alan" (* 2 500))
      (list "barb" 2000)
      (list (string-append "carl" ", the great") 1500)
      (list "dawn" 2300))

(define title "ratings")

(check-expect `(html (head (title ,title)) (body (h1 ,title) (p "A Second web page")))
              (list 'html
                    (list 'head (list 'title "ratings"))
                    (list 'body (list 'h1 "ratings") (list 'p "A Second web page"))))

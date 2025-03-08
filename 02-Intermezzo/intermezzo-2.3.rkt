#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname intermezzo-2.3) (read-case-sensitive #t)
                          (teachpacks ())
                          (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;;; Intermezzo 2: Unquote Splice

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))

;; `(tr ,(make-row '(3 4 5)))
;; `(tr ,@(make-row '(3 4 5)))
;; (cons 'tr (make-row '(3 4 5)))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1")) (tr ,@(make-row row1)) (tr ,@(make-row row2))))

;; (make-table '(1 2 3 4 5) '(3.5 2.8 -1.1 1.3))

;; Ex-233

;1
(check-expect `(0 ,@'(1 2 3) 4) (list 0 1 2 3 4))

;2
(check-expect
 `(("alan" ,(* 2 500)) ("barb" 2000) (,@'("carl" " , the great") 1500) ("dawn" 2300))
 (list (list "alan" 1000) (list "barb" 2000) (list "carl" " , the great" 1500) (list "dawn" 2300)))

;3
(check-expect
 `(html (body (table ((border "1"))
                     (tr ((width "200")) ,@(make-row '(1 2)))
                     (tr ((width "200")) ,@(make-row '(99 65))))))
 (list 'html
       (list 'body
             (list 'table
                   (list (list 'border "1"))
                   (list 'tr (list (list 'width "200")) (list 'td "1") (list 'td "2"))
                   (list 'tr (list (list 'width "200")) (list 'td "99") (list 'td "65"))))))

;; Ex-234

(define one-list '("Asia: Heat of the Moment" "U2: One" "The White Stripes: Seven Nation Army"))

(define (ranking los)
  (reverse (add-ranks (reverse los))))

(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los)) (add-ranks (rest los)))]))

;; (define (make-ranking ls)
;;   `(html (body (table ))))

(define (rank-table ls)
  (cond
    [(empty? ls) '()]
    [else `(td ,@(first (ranking ls)) ,(rank-table (rest ls)))]))

(list 'td 1 "Asia: Heat of the Moment"
      (list 'td 1 "U2: One"
            (list 'td 1 "The White Stripes: Seven Nation Army" '())))

(list 'td 1 "Asia: Heat of the Moment" 'td 1 "U2: One" 'td 1 "The White Stripes: Seven Nation Army")
(list 'td
      (list 1 "Asia: Heat of the Moment")
      'td
      (list 1 "U2: One")
      'td
      (list 1 "The White Stripes: Seven Nation Army"))
(list 'td
      (list 1 "Asia: Heat of the Moment")
      (list 'td (list 1 "U2: One") (list 'td (list 1 "The White Stripes: Seven Nation Army") '())))
(list (list 'td
            (list 1 "Asia: Heat of the Moment")
            (list (list 'td
                        (list 1 "U2: One")
                        (list (list 'td (list 1 "The White Stripes: Seven Nation Army") '()))))))

;; (list (list 1 "Asia: Heat of the Moment")
;;       (list 2 "U2: One")
;;       (list 3 "The White Stripes: Seven Nation Army"))

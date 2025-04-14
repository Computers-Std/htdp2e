#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fig-97) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define-struct address [first-name last-name street])
; An Addr is a structure:
; (make-address String String String)
; interpretation associates an address with a person's name

; [List-of Addr] -> String
; creates a string from first names,
; sorted in a alphabetical order,
; seperated and surrounded by blank spaces
(define (listing l)
  (foldr string-append-with-space " "
         (sort (map address-first-name l) string<?)))

; String String -> String
; appends two strings, prefixes with " "
(define (string-append-with-space s t)
  (string-append " " s t))

(define ex0
  (list (make-address "Robert" "Finlder" "South")
        (make-address "Matthew" "Flatt" "Canyon")
        (make-address "Shriram" "Krishna" "Yellow")
        (make-address "Usha" "Kiran" "Vizag")))

(check-expect (listing ex0) " Matthew Robert Shriram Usha ")


; Composing MyFuncs

;  [List-of X] Operation -> [List-of Y]
(check-expect (my-map address-first-name ex0)
              (list "Robert" "Matthew" "Shriram" "Usha"))
(check-expect (my-map add1 (list 1 2 2 3 3 3))
              (list 2 3 3 4 4 4))
(define (my-map opr la)
  (cond
    [(empty? la) '()]
    [else (cons (opr (first la))
                (my-map opr (rest la)))]))

; insert string
; X [List-of X] Operation -> [List-of X]
(check-expect (insert "bbb" (list "aaa" "ccc") string<?)
              (list "aaa" "bbb" "ccc"))
(check-expect (insert "bbb" (list "aaa" "ccc") string>?)
              (list "bbb" "aaa" "ccc"))
(check-expect (insert 4 (list 2 4 6) >) (list 4 2 4 6))
(check-expect (insert 4 (list 2 4 6) <) (list 2 4 4 6))
(define (insert s los opr)
  (cond
    [(empty? los) (cons s '())]
    [else (if (opr s (first los))
              (cons s los)
              (cons (first los)
                    (insert s (rest los) opr)))]))

;; ; [List-of X] Operation -> [List-of X]
(check-expect (my-sort (list "bbb" "ccc" "aaa") string<?) (list "aaa" "bbb" "ccc"))
(check-expect (my-sort (list 2 34 56 20) <) (list 2 20 34 56))
(define (my-sort los opr)
  (cond
    [(empty? los) '()]
    [(cons? los) (insert (first los)
                         (my-sort (rest los) opr)
                         opr)]))

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))

;; (define (my-foldr in li)
;;   (cond
;;     [(empty? li) in]
;;     [else (+ in ())]))

;; TODO: implement `foldr' from ground up

;; (define (my-foldr opr in li)
;;   (cond
;;     [(empty? li) in]
;;     [else (opr in (first (reverse li))
;;                (my-foldr opr in (rest (reverse li))))]))

(define (my-listing l)
  (foldr string-append-with-space " "
         (my-sort (my-map address-first-name l) string<?)))

(check-expect (listing ex0) (my-listing ex0))

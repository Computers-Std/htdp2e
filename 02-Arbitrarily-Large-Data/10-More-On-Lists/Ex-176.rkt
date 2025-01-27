#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-176) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket


;;; Predefined

; Matrix -> Matrix
; transpose the given matrix along the diagonal

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(check-expect (transpose mat1) tam1)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; A Matrix is one of:
; - (cons Row '())
; - (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
; - '()
; - (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; - Auxiliary functions (first* and rest*)

; Matrix -> List-of-Number
; interpretation: consumes a matrix and
; produces the first column as a list of numbers
(check-expect (first* mat1) (cons 11 (cons 21 '())))
(define (first* mat)
  (cond
    [(empty? mat) '()]
    [else (cons (first (first mat))
                (first* (rest mat)))]))

; Matrix -> Matrix
; interpretation: consumes a matrix and
; removes the first column. The result is a matrix
(check-expect (rest* mat1) (cons (cons 12 '())
                                 (cons (cons 22 '()) '())))
(define (rest* mat)
  (cond
    [(empty? mat) '()]
    [else (cons (rest (first mat))
                (rest* (rest mat)))]))

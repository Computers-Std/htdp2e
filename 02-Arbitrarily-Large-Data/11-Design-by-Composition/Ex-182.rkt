#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-182) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 182. Use cons and '() to form the equivalent of these lists:

;; (list 0 1 2 3 4 5)

;; (list (list "he" 0) (list "it" 1) (list "lui" 14))

;; (list 1 (list 1 2) (list 1 2 3))

;; Use check-expect to express your answers.

(check-expect 1-old 1-new)
(define 1-old
  (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(define 1-new
  (list 0 1 2 3 4 5))

(check-expect 2-old 2-new)
(define 2-new
  (list (list "he" 0) (list "it" 1) (list "lui" 14)))
(define 2-old
  (cons (cons "he" (cons 0 '()))
        (cons (cons "it" (cons 1 '()))
              (cons (cons "lui" (cons 14 '()))
                    '()))))

(check-expect 3-old 3-new)
(define 3-new
  (list 1
        (list 1 2)
        (list 1 2 3)))
(define 3-old
  (cons 1
        (cons (cons 1 (cons 2 '()))
              (cons (cons 1 (cons 2 (cons 3 '()))) '()))))

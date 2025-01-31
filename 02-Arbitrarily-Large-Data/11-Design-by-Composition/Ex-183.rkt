#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-183) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 183. On some occasions lists are formed with cons and list.

;; (cons "a" (list 0 #false))

;; (list (cons 1 (cons 13 '())))

;; (cons (list 1 (list 13 '())) '())

;; (list '() '() (cons 1 '()))

;; (cons "a" (cons (list 1) (list #false '())))

;; Reformulate each of the following expressions using only cons or
;; only list. Use check-expect to check your answers.

(check-expect q1 a1)
(define q1 (cons "a" (list 0 #false)))
(define a1 (list"a" 0 #false))

(check-expect q2 a2)
(define q2 (list (cons 1 (cons 13 '()))))
(define a2 (list (list 1 13)))

(check-expect q3 a3)
(define q3 (cons (list 1 (list 13 '())) '()))
(define a3 (list (list 1 (list 13 '()))))

(check-expect q4 a4)
(define q4 (list '() '() (cons 1 '())))
(define a4 (list '() '() (list 1)))

(check-expect q5 a5)
(define q5 (cons "a" (cons (list 1) (list #false '()))))
(define a5 (list "a" (list 1) #false '()))

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-184) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 184. Determine the values of the following expressions:

;; (list (string=? "a" "b") #false)

;; (list (+ 10 20) (* 10 20) (/ 10 20))

;; (list "dana" "jane" "mary" "laura")

;; Use check-expect to express your answers

(check-expect q1 a1)
(define q1 (list (string=? "a" "b") #false))
(define a1 (list #false #false))

(check-expect q2 a2)
(define q2 (list (+ 10 20) (* 10 20) (/ 10 20)))
(define a2 (list 30 200 1/2))

(define q3 (list "dana" "jane" "mary" "laura"))
(define a3 (list "dana" "jane" "mary" "laura"))

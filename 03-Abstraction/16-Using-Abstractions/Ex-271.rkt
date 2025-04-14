#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-271) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
;; (require racket/string)
;; Exercise 271. Use ormap to define find-name. The function consumes
;; a name and a list of names. It determines whether any of the names
;; on the latter are equal to or an extension of the former.

;; With andmap you can define a function that checks all names on a
;; list of names that start with the letter "a".

;; Should you use ormap or andmap to define a function that ensures
;; that no name on some list exceeds a given width?

; ormap
; String [List-of String] -> Boolean
(check-expect (find-name "kiran" (list "ushakiran" "azusa" "akari")) #true)
(check-expect (find-name "oribe" (list "ushakiran" "azusa oribe" "akari")) #true)
(check-expect (find-name "takashi" (list "ushakiran" "azusa oribe" "akari")) #false)
(define (find-name name ln)
  (local (; [List-of String]
          (define (similar? n)
            (string-contains? name n)))
    (ormap similar? ln)))

; andmap
; String [List-of String] -> Boolean
; X [List-of X] -> Boolean
(check-expect (start-with "k" (list "ushakiran" "azusa" "akari")) #false)
(check-expect (start-with "a" (list "amma" "azusa oribe" "akari")) #true)
(define (start-with l ls)
  (local (; [List-of String]
          (define (start? s)
            (string=?
             (string-ith s 0)
             l)))
    (andmap start? ls)))


; Is ormap, andmap necessary for our needs?
; not much, we can use recursion to pass through every item in the
; list

(check-expect (start-with2 "ka" (list "ushakiran" "azusa" "akari")) #false)
(check-expect (start-with2 "aa" (list "aamma" "aaazusa oribe" "aaakari")) #true)
; String [List-of String] -> Boolean
(define (start-with2 l ls)
  (local (; [List-of String]
          (define (start? s)
            (and
             (> (string-length s) (string-length l))
             (string=? l (substring s 0 (string-length l))))))
    (cond
      [(empty? ls) #true]
      [else (and (start? (first ls))
                 (start-with2 l (rest ls)))])))

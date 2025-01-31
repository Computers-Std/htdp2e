#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 188. Design a program that sorts lists of emails by date:

(define-struct email [from date message])
; An Email Message is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

; List-of-emails is one of:
; - '()
; - (cons email List-of-emails)

; Sort Email from Newst to Oldest
(define (sort-em> loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe) (insert-em (first loe)
                            (sort-em> (rest loe)))]))

; Email List-of-emails -> List-of-emails
(define (insert-em em loe)
  (cond
    [(empty? loe) (cons em '())]
    [else (if (>= (email-date em) (email-date (first loe)))
              (cons em loe)
              (cons (first loe) (insert-em em (rest loe))))]))

; Sort Emails by name (alphabetically)
; List-of-emails -> List-of-emails
(define (sort/name> loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe) (insert-nm (first loe)
                            (sort/name> (rest loe)))]))

; Email List-of-emails -> List-of-emails
; inserts Email in the sorted(alphabetically) List-of-emails
(define (insert-nm em loe)
  (cond
    [(empty? loe) (cons em '())]
    [else (if (string<? (email-from em) (email-from (first loe)))
              (cons em loe)
              (cons (first loe) (insert-nm em (rest loe))))]))

;; - Testing Area
(define loe1
  (list (make-email "from1" 20231015 "message1")
        (make-email "from2" 19981225 "message2")
        (make-email "from3" 20250228 "message3")
        (make-email "from4" 19850704 "message4")
        (make-email "from5" 20231101 "message5")))

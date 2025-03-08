#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-234) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/web-io)

;; Example list
(define one-list '("Asia: Heat of the Moment"
                   "U2: One"
                   "The White Stripes: Seven Nation Army"))

; List-of-strings -> ... nestd list ...
; make a list
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> ... nested list ...
; make a list of lists
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los)) (add-ranks (rest los)))]))

; Number or String -> ... nested list ...
; make a single cell
(define (make-cell nos)
  (if (string? nos)
      `(td ,nos)
      `(td ,(number->string nos))))

; Song -> ... nested list ...
; make a single row for the given song(list)
(define (make-a-row ls)
  (cond
    [(empty? ls) '()]
    [else (cons (make-cell (first ls)) (make-a-row (rest ls)))]))

; Songs -> ... nested list ...
; make rows for given no. of songs(lists)
(define (make-rows ls)
  (cond
    [(empty? ls) '()]
    [else (cons `(tr ,@(make-a-row (first ls))) (make-rows (rest ls)))]))

; Songs List -> ... nested list ...
(define (make-ranking ls)
  `(html (head (title "Top 3 Songs from the 80s 90s 00s")
               (body (table ((border "1")) ,@(make-rows (ranking ls)))))))

;; (make-ranking one-list)
;; (show-in-browser (make-ranking one-list))

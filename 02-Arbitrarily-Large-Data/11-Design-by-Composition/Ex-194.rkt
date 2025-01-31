#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-194) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; a plain background image
(define MT (empty-scene 50 50))

; An NELoP is one of:
; – (cons Posn '())
; – (cons Posn NELoP)

; --- Testing Area
(define triangle-p (list (make-posn 20 10) (make-posn 20 20)
                         (make-posn 30 20)))
(define square-p (list (make-posn 10 10) (make-posn 20 10)
                       (make-posn 20 20) (make-posn 10 20)))
(define pentagon-p (cons (make-posn 40 30) square-p))

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT square-p (make-posn 10 10))
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))
(define (connect-dots img p pos)
  (cond
    [(empty? (rest p)) (render-line img (first p) pos)]
    [else (render-line (connect-dots img (rest p) pos)
                       (first p)
                       (second p))]))

;; INSIGHT: When there is a well-written recursive function, and then,
;; if needed, to do
;; something on the First Step: (cons Step (old list)) and make it a
;; circle;
;; something on the Last Step: at [(empty? list) then-do], where:
;; then-do => Step ++ then-do.

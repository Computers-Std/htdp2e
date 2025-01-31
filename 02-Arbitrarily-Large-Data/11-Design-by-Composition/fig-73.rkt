#reader(lib "htdp-beginner-reader.ss" "lang")((modname fig-73) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; a plain background image
(define MT (empty-scene 50 50))

; An NELoP is one of:
; – (cons Posn '())
; – (cons Posn NELoP)

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))

; Image Polygon -> Image
; adds an image of p to img
(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; - Testing Area

(define triangle-p (list (make-posn 20 10) (make-posn 20 20)
                         (make-posn 30 20)))
(define square-p (list (make-posn 10 10) (make-posn 20 10)
                       (make-posn 20 20) (make-posn 10 20)))
(define pentagon-p (cons (make-posn 40 30) square-p))

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname polygon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(check-expect (render-line MT (make-posn 10 20) (make-posn 20 30))
              (scene+line MT 10 20 20 30 "red"))
(check-expect (render-line MT (make-posn 20 30) (make-posn 30 40))
              (scene+line MT 20 30 30 40 "red"))
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; Image Polygon -> Image
; renders the given polygon p into img
(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
      (render-line
       (render-line MT (first p) (second p))
       (second p) (third p))
      (third p) (first p))]
    [else (render-line
           (render-poly img (rest p))
           (first p)
           (second p))]))

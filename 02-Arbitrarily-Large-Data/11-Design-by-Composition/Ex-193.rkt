#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-193) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))

; Image Polygon -> Image
; adds an image of p to img
; Question 1
(check-expect (render-poly-q1 MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))
(define (render-poly-q1 img p)
  (connect-dots img (cons (last p) p)))

; Image Polygon -> Image
; adds an image of p to img
; Question 2
(check-expect (render-poly-q2 MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))
(define (render-poly-q2 img p)
  (connect-dots img (add-to-end p (first p))))

; Lo1S 1String -> Lo1S
; create a new list by adding s to the end of l
(check-expect (add-to-end (cons "c" (cons "b" '())) "a")
              (cons "c" (cons "b" (cons "a" '()))))
(check-expect (add-to-end triangle-p (make-posn 20 40))
              (cons (make-posn 20 10)
                    (cons (make-posn 20 20) (cons (make-posn 30 20)
                                                  (cons (make-posn 20 40) '())))))
(define (add-to-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l)
                (add-to-end (rest l) s))]))

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-258) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; a plain background image
(define MT (empty-scene 50 50))

; An NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

; Image Polygon -> Image
; adds a corner of p to img
(define (render-polygon img0 p0)
  (local (
          ; Image NELoP ->Image
          ; connect all edges of polygon onto an img
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))
          ; NELoP -> Posn
          ; extracts the last edge of polygon
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))
          ; Image Posn -> Image
          ; connects the last and first edges of polygon onto an img
          (define (full-polygon img p)
            (render-line (connect-dots img p) (first p) (last p))))
    (full-polygon img0 p0)))

; Image Posn Posn -> Image
; draw a red line from Posn p to Posn q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; - Testing Area
(define triangle-p (list (make-posn 20 10) (make-posn 20 20)
                         (make-posn 30 20)))
(define square-p (list (make-posn 10 10) (make-posn 20 10)
                       (make-posn 20 20) (make-posn 10 20)))
(define pentagon-p (cons (make-posn 40 30) square-p))

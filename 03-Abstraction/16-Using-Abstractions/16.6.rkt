#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 16.6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)

; Constants
(define DOT (circle 5 "solid" "red"))
(define MT-SCENE (empty-scene 200 200))

; [List-of Posn] -> Image
; adds the Posn on lop to the empty scene
(define (dots lop)
  (local (; Posn Image -> Image
          (define (add-one-dot p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    ;; (foldr add-one-dot MT-SCENE lop) ; OR
    (foldl add-one-dot MT-SCENE lop)))

;; Testing
(define lop1 (list (make-posn 10 20)
                   (make-posn 20 30)
                   (make-posn 30 40)))

; Application
;; (dots lop1)

;; Observation
; foldr is faster than foldl, as it starts from the last item of the
; list

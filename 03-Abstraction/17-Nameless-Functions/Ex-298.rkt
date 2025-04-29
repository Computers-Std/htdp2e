#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

(define rocket (bitmap "./images/rocket.png"))

; An ImageStream is a function:
; [N -> Image]
; interpretation: a stream s denotes a series of images

; ImageStream (Example)
(define (create-rocket-scene height)
  (place-image rocket 50 height (empty-scene 60 60)))

; ImageStream N -> Number
; Shows the images (s, 0), (s, 1) and so on
; at a rate of 30 imgs per sec up to n images total
(define (my-animate s n0)
  (big-bang 0
            [to-draw s]
            [on-tick add1 1/30]
            [stop-when (lambda (n) (= n n0))]))

;; (my-animate (lambda (n)
;;               (place-image rocket 30 n (empty-scene 60 60))) 10)

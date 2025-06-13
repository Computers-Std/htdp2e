#lang htdp/isl+

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

; [List-of Number] -> Number
(define (sum lon)
  (foldl + 0 lon))
;; (foldr (lambda (n total) (+ n total)) 0 lon)

(sum JANUS)
; #i99.0

(sum (reverse JANUS))
; #i-1.2345678901235e+80

(sum (sort JANUS <))
; #i0.0

(exact->inexact (sum (map inexact->exact JANUS)))
; #i4.2344294738446e+170

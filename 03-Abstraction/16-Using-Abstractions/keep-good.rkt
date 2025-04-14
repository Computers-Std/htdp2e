#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname keep-good) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

; [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y-coordinate is > 100
(check-expect (keep-good (list (make-posn 0 100) (make-posn 0 60)))
              (list (make-posn 0 60)))
(define (keep-good lop)
  (local (; Posn -> Posn
          ; should this Posn stay on the list
          (define (good? p)
            (if (< (posn-y p) 100)
                #true #false)))
    (filter good? lop)))

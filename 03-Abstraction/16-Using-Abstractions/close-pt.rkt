#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname close-pt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Sample Problem Design a function that determines whether any of a
;; list of Posns is close to some given position pt where “close”
;; means a distance of at most 5 pixels.

; Constants
(define CLOSENESS 5)

; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(check-expect (close-to? (make-posn 2 3) (make-posn 3 4) 4) #true)
(define (close-to? p q d)
  (if (>= d (dist p q))
      #true #false))

; Posn Posn -> Number
; distance b/w given points
(check-expect (dist (make-posn 2 3) (make-posn 3 7)) 4)
(define (dist p q)
  (integer-sqrt (+ (expt (- (posn-x q) (posn-x p)) 2)
                   (expt (- (posn-y q) (posn-y p)) 2))))


; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt
(check-expect (close? (list (make-posn 47 54)
                            (make-posn 0 60))
                      (make-posn 50 50))
              #true)
(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to? p pt CLOSENESS)))
    (ormap is-one-close? lop)))

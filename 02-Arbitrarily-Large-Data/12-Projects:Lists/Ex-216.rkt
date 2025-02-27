#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-216) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

;; Graphical Constants
; worm segment (disk) diameter
(define DIAMETER 10)
; worm segment color
(define SEG-COLOR "red")
(define WORM (circle DIAMETER "solid" SEG-COLOR))
(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FINAL-SCN-X (/ HEIGHT 2))
(define FINAL-SCN-Y (* WIDTH 3/4))
(define FINAL-TEXT (text "worm hit border" 18 "orange"))
; Data Definitions

; A Physical Representation

; A SEGMENT is a Posn Structure
; interpretation: (make-posn x y)

; A DIRECTION is one of:
; L, R, U, D
; Left, Right, Up, Down

; A LENGTH is Positive Number

; A WORM is a SEGMENT, DIRECTION, LENGTH
(define-struct worm [seg dir len])
; interpretation: (make-worm [(make-posn x y) dir])
; is WORM's location and movement

; WormState: data representing the current Worm (ws)

; WORM Image -> Image
; when needed, big-bang obtains the image of the current
; state of the worm by evaluating (render wm)
(define (render ws)
  (place-image WORM (posn-x (worm-seg ws)) (posn-y (worm-seg ws)) BACKGROUND))

; WormState -> WormState
; for each tick of the clock, big-bang obtains the next state of the
; worm from (tick-handler ws)
(check-expect (tick-handler worm1) (make-worm (make-posn 10 20) "U" 10))
(check-expect (tick-handler worm2) (make-worm (make-posn 0 20) "R" 2))
(define (tick-handler ws)
  (make-worm (cond
               [(and (string=? "L" (worm-dir ws)) (not (isToched? ws)))
                (make-posn (- (posn-x (worm-seg ws)) DIAMETER) (posn-y (worm-seg ws)))]
               [(and (string=? "R" (worm-dir ws)) (not (isToched? ws)))
                (make-posn (+ (posn-x (worm-seg ws)) DIAMETER) (posn-y (worm-seg ws)))]
               [(and (string=? "U" (worm-dir ws)) (not (isToched? ws)))
                (make-posn (posn-x (worm-seg ws)) (- (posn-y (worm-seg ws)) DIAMETER))]
               [(and (string=? "D" (worm-dir ws)) (not (isToched? ws)))
                (make-posn (posn-x (worm-seg ws)) (+ (posn-y (worm-seg ws)) DIAMETER))]
               [else (make-posn (posn-x (worm-seg ws)) (posn-y (worm-seg ws)))])
             (worm-dir ws)
             (worm-len ws)))

; WormState -> Boolean
; Is the Worm touched the wall
(define (isToched? ws)
  (if (or (<= (posn-x (worm-seg ws)) DIAMETER)
          (>= (posn-x (worm-seg ws)) WIDTH)
          (<= (posn-y (worm-seg ws)) DIAMETER)
          (>= (posn-y (worm-seg ws)) HEIGHT))
      #true
      #false))

; WormState String -> WormState
; for each keystroke, big-bang obtains the next state from
; (key-handler ws ke); ke represents the key
(check-expect (key-handler worm1 "up") (make-worm (make-posn 10 20) "U" 10))
(check-expect (key-handler worm2 "left") worm2)
(define (key-handler ws ke)
  (if (not (isToched? ws))
      (make-worm (make-posn (posn-x (worm-seg ws)) (posn-y (worm-seg ws)))
                 (cond
                   [(key=? ke "left") "L"]
                   [(key=? ke "right") "R"]
                   [(key=? ke "up") "U"]
                   [(key=? ke "down") "D"])
                 (worm-len ws))
      ws))

; WormState -> Image
; Game ends, as the worm touched the wall
(check-expect (game-over? worm1) #true)
(check-expect (game-over? df-worm) #false)
(check-expect (game-over? worm2) #true)
(define (game-over? ws)
  (isToched? ws))

; Image -> Image
; Final Scene of the game
(define (final-scene img)
  (place-image FINAL-TEXT FINAL-SCN-X FINAL-SCN-Y (render img)))

; Main
(define (snake-game ws)
  (big-bang ws
            [to-draw render]
            [on-tick tick-handler 1]
            [on-key key-handler]
            [stop-when game-over? final-scene]))

;; Testing Area
(define worm0 (make-worm (make-posn 0 20) "U" 10))
(define worm1 (make-worm (make-posn 10 20) "U" 10))
(define worm2 (make-worm (make-posn 0 20) "R" 2))
(define df-worm (make-worm (make-posn 50 50) "R" 1))

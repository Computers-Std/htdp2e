#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-217) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

;; Graphical Constants
; worm segment (disk) diameter
(define DIAMETER 10)
; worm segment color
(define SEG-COLOR "red")
(define WORM (circle (/ DIAMETER 2) "solid" SEG-COLOR))
(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FINAL-TEXT (text "worm hit border" 18 "orange"))

; Data Definitions

; A Physical Representation

; A SEGMENT is a Posn Structure
; interpretation: (make-posn x y)

; A DIRECTION is one of:
; L, R, U, D
; Left, Right, Up, Down

; TAIL is sequence of connected (coordinates of a segment differ from
; predecessor) segments

; A WORM is a SEGMENT, DIRECTION, TAIL
(define-struct worm [seg dir tail])
; interpretation: (make-worm (make-posn x y) dir tail)
; is WORM's location, movement and tail

; Examples
(define WORM1 (make-worm (make-posn (/ WIDTH 2) (/ HEIGHT 2)) "R" '()))
(define WORM2
  (make-worm (make-posn 100 150)
             "U"
             (list (make-posn 100 (- 150 DIAMETER)) (make-posn (- 100 DIAMETER) (- 150 DIAMETER)))))

; Render
; WormState -> Image
(define (render ws)
  (place-images (make-list (add1 (length (worm-tail ws))) WORM)
                (append (list (worm-seg ws)) (worm-tail ws))
                BACKGROUND))

; WormState -> WormState
; tick-handler
(define (tick-handler ws)
  (make-worm (head-move (worm-seg ws) (worm-dir ws) DIAMETER)
             (worm-dir ws)
             (tail-move (worm-seg ws) (worm-tail ws))))

; Posn Direction PositiveNumber -> Posn
; move the Head-Seg in given Direction
(define (head-move seg dir d)
  (cond
    [(string=? dir "U") (make-posn (posn-x seg) (- (posn-y seg) d))]
    [(string=? dir "D") (make-posn (posn-x seg) (+ (posn-y seg) d))]
    [(string=? dir "R") (make-posn (+ (posn-x seg) d) (posn-y seg))]
    [(string=? dir "L") (make-posn (- (posn-x seg) d) (posn-y seg))]))

; List-of-Posns Posn -> List-of-Posns
; Prepend the Head and remove the last Tail segment
(define (tail-move head tail)
  (cond
    [(empty? tail) '()]
    [else (cons head (reverse (rest (reverse tail))))]))

; WormState -> Boolean
; Is the Worm touched the wall
(define (isToched? ws)
  (if (or (< (posn-x (worm-seg ws)) DIAMETER)
          (> (posn-x (worm-seg ws)) WIDTH)
          (< (posn-y (worm-seg ws)) DIAMETER)
          (> (posn-y (worm-seg ws)) HEIGHT))
      #true
      #false))

; WormState String -> WormState
; for each keystroke, big-bang obtains the next state from
; (key-handler ws ke); ke represents the key
;; (check-expect (key-handler WORM2 "right")
;; (make-worm (make-posn 100 100) "R" (list (make-posn 100 90) (make-posn 90 90))))
(check-expect (key-handler WORM1 "down") (make-worm (make-posn 100 100) "D" '()))
(define (key-handler ws ke)
  (if (not (isToched? ws))
      (make-worm (make-posn (posn-x (worm-seg ws)) (posn-y (worm-seg ws)))
                 (cond
                   [(key=? ke "left") "L"]
                   [(key=? ke "right") "R"]
                   [(key=? ke "up") "U"]
                   [(key=? ke "down") "D"])
                 (worm-tail ws)) ws))

; WormState -> Image
; Game ends, as the worm touched the wall
(define (game-over? ws)
  (isToched? ws))

; Image -> Image
; Final Scene of the game
(define (final-scene ws)
  (overlay/align/offset "left" "bottom"
                        FINAL-TEXT
                        -10 5
                        (render ws)))

; Main
(define (main ws)
  (big-bang ws
            [to-draw render]
            [on-tick tick-handler 1]
            [on-key key-handler]
            [stop-when game-over? final-scene]))

; Theory: As this exercise was a challenging one for me, I try to
; bring out how it clicked

;; Hint One way to realize the worm’s movement is to add a segment in
;; the direction in which it is moving and to delete the last one

; default: head tail
; on start     ==> h0 (list t1 t2)
; after a tick ==> h1 (list h0 t1)
;              ==> h2 (list h1 h0) .. so on., the snake moves

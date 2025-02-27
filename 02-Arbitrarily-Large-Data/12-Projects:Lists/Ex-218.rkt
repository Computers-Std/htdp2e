#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-218) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 218. Redesign your program from exercise 217 so that it
;; stops if the worm has run into the walls of the world or into
;; itself

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

; Data Definitions

; A Physical Representation

; A HEAD is a Posn Structure
; interpretation: (make-posn x y)

; A DIRECTION is one of:
; L, R, U, D
; Left, Right, Up, Down

; TAIL is sequence of connected (coordinates of a segment differ from
; predecessor) segments

; A WORM is a HEAD, DIRECTION, TAIL
(define-struct worm [head dir tail])
; interpretation: (make-worm (make-posn x y) dir tail)
; is WORM's location, movement and tail

; Examples
(define WORM1 (make-worm (make-posn (/ WIDTH 2) (/ HEIGHT 2)) "R" '()))
(define WORM2
  (make-worm (make-posn 100 150)
             "U"
             (list (make-posn 100 (- 150 DIAMETER)) (make-posn (- 100 DIAMETER) (- 150 DIAMETER)))))
(define WORM3
  (make-worm (make-posn 100 150)
             "D"
             (list (make-posn 100 (- 150 DIAMETER))
                   (make-posn 100 (- 150 (* 2 DIAMETER)))
                   (make-posn 100 (- 150 (* 3 DIAMETER))))))

; Render
; WormState -> Image
(define (render ws)
  (place-images (make-list (add1 (length (worm-tail ws))) WORM)
                (append (list (worm-head ws)) (worm-tail ws))
                BACKGROUND))

; WormState -> WormState
; tick-handler
(define (tick-handler ws)
  (make-worm (head-move (worm-head ws) (worm-dir ws) DIAMETER)
             (worm-dir ws)
             (tail-move (worm-head ws) (worm-tail ws))))

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
  (if (or (< (posn-x (worm-head ws)) DIAMETER)
          (> (posn-x (worm-head ws)) WIDTH)
          (< (posn-y (worm-head ws)) DIAMETER)
          (> (posn-y (worm-head ws)) HEIGHT))
      #true
      #false))

; WormState -> Boolean
; Is the Worm crossed itself
(check-expect
 (isCorssed? (make-worm (make-posn 100 160)
                        "R"
                        (list (make-posn 90 160) (make-posn 100 160) (make-posn 100 150))))
 #true)
(define (isCorssed? ws)
  (if (member? (worm-head ws) (worm-tail ws)) #true #false))

; WormState String -> WormState
; for each keystroke, big-bang obtains the next state from
; (key-handler ws ke); ke represents the key
(check-expect (key-handler WORM1 "down") (make-worm (make-posn 100 100) "D" '()))
(define (key-handler ws ke)
  (if (not (isToched? ws))
      (make-worm (make-posn (posn-x (worm-head ws)) (posn-y (worm-head ws)))
                 (cond
                   [(key=? ke "left") "L"]
                   [(key=? ke "right") "R"]
                   [(key=? ke "up") "U"]
                   [(key=? ke "down") "D"])
                 (worm-tail ws))
      ws))

; WormState -> Image
; Game ends,
; If  the worm touched the wall or Crossed itself
(define (game-over? ws)
  (or (isToched? ws) (isCorssed? ws)))

; Image -> Image
; Final Scene of the game
(define (final-scene ws)
  (if (isToched? ws)
      (overlay/align/offset "left" "bottom" (text "worm hit border" 18 "orange") -10 5 (render ws))
      (overlay/align/offset "left"
                            "bottom"
                            (text "worm crossed itself!" 18 "orange")
                            -10
                            5
                            (render ws))))

; Main
(define (main ws)
  (big-bang ws
            [to-draw render]
            [on-tick tick-handler 1]
            [on-key key-handler]
            [stop-when game-over? final-scene]))

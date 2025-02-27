#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-219) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/image)
(require 2htdp/universe)

; Exercise 219. Equip your program from exercise 218 with food. At any
; point in time, the box should contain one piece of food.

;; Graphical Constants
; worm segment (disk) diameter
(define DIAMETER 10)
; worm segment color
(define SEG-COLOR "red")
(define FOOD-COLOR "green")
(define WORM (circle (/ DIAMETER 2) "solid" SEG-COLOR))
(define FOOD (circle (/ DIAMETER 2) "solid" FOOD-COLOR))
(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; A WORM is a HEAD, DIRECTION, TAIL, FOOD
; Head -> Posn
; Direction -> String
; Tail -> List-of-posns
; Food -> Posn
(define-struct worm [head dir tail food])

; Examples
(define WORM1 (make-worm (make-posn (/ WIDTH 2) (/ HEIGHT 2)) "R" '() (make-posn 20 30)))
(define WORM2
  (make-worm (make-posn 100 150)
             "R"
             (list (make-posn 100 (- 150 DIAMETER)) (make-posn 100 (- 150 (* 2 DIAMETER))))
             (make-posn 40 60)))
(define WORM3
  (make-worm (make-posn 100 150)
             "L"
             (list (make-posn 100 (- 150 DIAMETER))
                   (make-posn 100 (- 150 (* 2 DIAMETER)))
                   (make-posn 100 (- 150 (* 3 DIAMETER))))
             (make-posn 30 30)))
(define WORM4
  (make-worm (make-posn 100 150)
             "R"
             (list (make-posn 100 (- 150 DIAMETER)) (make-posn 100 (- 150 (* 2 DIAMETER))))
             (make-posn 100 150)))
; Render
; WormState -> Image
(define (render ws)
  (place-images (append (list FOOD) (make-list (add1 (length (worm-tail ws))) WORM))
                (append (list (worm-food ws)) (list (worm-head ws)) (worm-tail ws))
                BACKGROUND))

; WormState -> WormState
; tick-handler
(define (tick-handler ws)
  (make-worm
   (head-move (worm-head ws) (worm-dir ws) DIAMETER)
   (worm-dir ws)
   (if (isEaten? ws)
       (add-tail (worm-head ws) (worm-tail ws))
       (tail-move (worm-head ws) (worm-tail ws)))
   (worm-food ws)))

(define (tick-handler2 ws)
  (cond
    [(isEaten? ws) (make-worm (worm-food ws)
                              (worm-dir ws)
                              (add-tail (worm-head ws) (worm-tail ws))
                              (food-create (worm-food ws)))]
    [else (make-worm (head-move (worm-head ws) (worm-dir ws) DIAMETER)
                     (worm-dir ws)
                     (tail-move (worm-head ws) (worm-tail ws))
                     (worm-food ws))]))

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

; WormState -> WormState
; Add a tail segment after eating food
(define (add-tail head tail)
  (cond
    [(empty? tail) (cons head '())]
    [else (cons head tail)]))

; WormState -> Boolean
; Is eaten food?
(check-expect (isEaten? WORM2) #false)
(check-expect (isEaten? WORM4) #true)
(define (isEaten? ws)
  (if (equal? (worm-head ws) (worm-food ws))
      #true #false))

; Posn -> Posn
; ???
;; (check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create pos)
  (food-check-create
   pos (make-posn rand-10 rand-10)))

; Posn Posn -> Posn
; generative recursion
; ???
(define (food-check-create pos candidate)
  (if (equal? pos candidate) (food-create pos) candidate))

; Produce a random 10 multiple
(define rand-10 (* 10 (random 10)))

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
(define (isCrossed? ws)
  (cond
    [(or (empty? (worm-tail ws)) (empty? (rest (worm-tail ws)))) #false]
    [(member? (worm-head ws) (rest (worm-tail ws))) #true]
    [else #false]))

; WormState String -> WormState
; for each keystroke, big-bang obtains the next state from
; (key-handler ws ke); ke represents the key
(check-expect (key-handler WORM1 "down") (make-worm (make-posn 100 100) "D" '() (make-posn 20 30)))
(define (key-handler ws ke)
  (if (not (isToched? ws))
      (make-worm (make-posn (posn-x (worm-head ws)) (posn-y (worm-head ws)))
                 (cond
                   [(key=? ke "left") "L"]
                   [(key=? ke "right") "R"]
                   [(key=? ke "up") "U"]
                   [(key=? ke "down") "D"])
                 (worm-tail ws)
                 (worm-food ws))
      ws))

; WormState -> Image
; Game ends,
; If  the worm touched the wall or Crossed itself
(define (game-over? ws)
  (or (isToched? ws) (isCrossed? ws)))

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
            [on-tick tick-handler2 1]
            [on-key key-handler]
            [stop-when game-over? final-scene]))

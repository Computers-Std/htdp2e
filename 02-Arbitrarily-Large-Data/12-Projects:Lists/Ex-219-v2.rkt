#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-219-v2) (read-case-sensitive #t)
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
(define GROUND (empty-scene WIDTH HEIGHT))

; A WORM is a HEAD, DIRECTION, TAIL
; Head -> Posn
; Direction -> String
; Tail -> List-of-posns
(define-struct worm [head dir tail])

; A GARDEN is a WORM, FOOD
; Food -> Posn
(define-struct garden [worm food])

; Examples
(define WORM0 (make-worm (make-posn (/ WIDTH 2) (/ HEIGHT 2)) "R" '()))
(define WORM1
  (make-worm (make-posn 100 150)
             "R"
             (list (make-posn 100 (- 150 DIAMETER)) (make-posn 100 (- 150 (* 2 DIAMETER))))))
(define WORM2
  (make-worm (make-posn 100 150)
             "L"
             (list (make-posn 100 (- 150 DIAMETER))
                   (make-posn 100 (- 150 (* 2 DIAMETER)))
                   (make-posn 100 (- 150 (* 3 DIAMETER))))))

(define GARDEN0 (make-garden WORM0 (make-posn (/ WIDTH 2) (/ HEIGHT 2)))) ; eating
(define GARDEN1 (make-garden WORM1 (make-posn 40 60)))
(define GARDEN2 (make-garden WORM2 (make-posn 30 30)))

; Render
; GardenState -> Image
(define (render gs)
  (place-images (make-list (add1 (length (worm-tail (garden-worm gs)))) WORM)
                (append (list (worm-head (garden-worm gs))) (worm-tail (garden-worm gs)))
                (place-image FOOD (posn-x (garden-food gs)) (posn-y (garden-food gs)) GROUND)))

; GardenState -> GardenState
; tick-handler
; moves Worm & manage Food & grows Worm
(define (tick-handler gs)
  (if (isEaten? (garden-worm gs))
      (gen-food (worm-grow (garden-worm gs)))
      (make-garden (worm-move (garden-worm gs)) (garden-food gs))))

(define (worm-move ws)
  (make-worm
   (head-move (worm-head ws) (worm-dir ws) DIAMETER)
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

; WormState -> WormState
(define (worm-grow ws)
  (make-worm (head-move (worm-head ws) (worm-dir ws) DIAMETER)
             (worm-dir ws)
             (tail-grow ws)))

; WormState -> List-of-Posns
(define (tail-grow ws)
  (cons (worm-head ws) (worm-tail ws)))

; GardenState -> Boolean
; Is eaten food?
(define (isEaten? gs)
  (if (equal? (worm-head (garden-worm gs)) (garden-food gs)) #true #false))

; generate random point on the GROUND
(define (rand-n n)
  (make-posn (* n (random 20)) (* n (random 20))))

; Generate Food

; WormState -> Garden
(define (gen-food ws)
  (make-garden ws (food-create (append (list (worm-head ws)) (worm-tail ws)))))

;; List-of-points -> Point
;; Produces the position of the food,
(define (food-create lop)
  (food-check-create lop (rand-n 10)))

;; List-of-posns Posn -> Posn
;; Generative recursion.
;; Returns the position of the food
;; or - if the candidate equals the snake's head location -
;; calls food-create for new generation attempt.
(check-expect (food-check-create (list (make-posn 10 10)) (make-posn 20 30)) (make-posn 20 30))
(define (food-check-create lop candidate)
  (if (member? candidate lop)
      (food-create lop)
      candidate))

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

; GardenState -> Image
; Game ends,
; If  the worm touched the wall or Crossed itself
(define (game-over? gs)
  (or (isToched? (garden-worm gs)) (isCrossed? (garden-worm gs))))

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

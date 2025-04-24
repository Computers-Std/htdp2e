#lang htdp/isl

(require 2htdp/universe)
(require 2htdp/image)

;;; Data and Constants Definitions

;; Classic navigation
(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")

;; A Course is one of these constants:
;; - UP
;; - DOWN
;; - LEFT
;; - RIGHT
;; Represents the direction of the move
;; of an object on the scene.

;; A Coord is one of:
;; - 0
;; - (+ 10 Coord)

;; A Point is a Posn with Coord x and y:
;;    (make-posn Coord Coord)
;; Represents a point of the world's grid.

(define-struct snake [loc course tail])
;; A Snake is a structure:
;;   (make-snake Point Course List-of-points)
;; (make-snake p c t)
;; represents a snake:
;; - with the head located on the point p
;; - with the tail t,
;; - that moves in the direction c.

(define-struct game [snake food])
;; A Game is a structure:
;;   (make-game Snake Point)
;; (make-game s f) represents a world state
;; with the snake s and the food position f.


;;; Constants

(define GAME-SPEED 0.4)

(define GRID-SIZE 10)
(define GRID-COLS 15)
(define GRID-ROWS 15)
(define SCENE-WIDTH (+ GRID-SIZE (* GRID-COLS GRID-SIZE)))
(define SCENE-HEIGHT (+ GRID-SIZE (* GRID-ROWS GRID-SIZE)))

(define SNAKE-RADIUS (/ GRID-SIZE 2))
(define SNAKE-SEGMENT (circle SNAKE-RADIUS "solid" "red"))
(define SNAKE-HEAD (circle SNAKE-RADIUS "outline" "maroon"))

(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "transparent"))

(define FOOD (circle SNAKE-RADIUS "solid" "green"))

(define FINAL-TEXT-SIZE GRID-SIZE)
(define FINAL-TEXT-COLOR "orange")
(define FINAL-TEXT-POSN (make-posn -5 5))


;;; Tests Constants

(define SNAKE-1 (make-snake (make-posn 50 50) RIGHT '()))
(define SNAKE-3 (make-snake (make-posn 100 150)
                            DOWN
                            (list (make-posn 100 (- 150 GRID-SIZE))
                                  (make-posn (- 100 GRID-SIZE) (- 150 GRID-SIZE)))))
(define SNAKE-5 (make-snake (make-posn 50 50)
                            UP
                            (list (make-posn 50 (+ 50 GRID-SIZE))
                                  (make-posn 50 (+ 50 (* 2 GRID-SIZE)))
                                  (make-posn 50 (+ 50 (* 3 GRID-SIZE)))
                                  (make-posn 50 (+ 50 (* 4 GRID-SIZE))))))
(define SNAKE-HIT (make-snake (make-posn 50 50) RIGHT
                              (list (make-posn (- 50 GRID-SIZE) 50)
                                    (make-posn (- 50 GRID-SIZE) (+ 50 GRID-SIZE))
                                    (make-posn 50 (+ 50 GRID-SIZE))
                                    (make-posn 50 50)
                                    (make-posn 50 (- 50 GRID-SIZE)))))
(define FOOD-INIT (make-posn (+ 50 (* 3 GRID-SIZE)) 50))
(define GAME-INIT (make-game SNAKE-1 FOOD-INIT))

;; Snake -> Image
;; Produces the image of the snake.
(define (draw-snake s)
  (place-image SNAKE-HEAD
               (posn-x (snake-loc s)) (posn-y (snake-loc s))
               (place-images
                (make-list (+ 1 (length (snake-tail s))) SNAKE-SEGMENT)
                (append (list (snake-loc s)) (snake-tail s))
                SCENE)))

;; Point -> Image
;; Produces the image of the food.
(define (draw-food p)
  (place-image FOOD (posn-x p) (posn-y p) SCENE))

;; Snake Point -> Boolean
;; Determines whether the snake's head is on the food's position.
(define (eat? s p)
  (member? (snake-loc s) (list p)))

;; Snake -> Game
;; Produces the game state after the eating.
(define (generate-food s)
  (make-game s (food-create (append (list (snake-loc s)) (snake-tail s)))))

;; Snake -> Snake
;; Moves the snake and enlarges its tail by one segment.
(define (grow s)
  (make-snake
   (move-head (snake-loc s) (snake-course s))
   (snake-course s)
   (grow-tail s)))

;; Snake -> List-of-points
;; Adds the head segment to the snake's tail.
(define (grow-tail s)
  (cons (snake-loc s) (snake-tail s)))

;; Snake -> Snake
;; Moves the snake.
(define (move s)
  (make-snake
   (move-head (snake-loc s) (snake-course s))
   (snake-course s)
   (move-tail (snake-tail s) (snake-loc s))))

;; Point Course -> Point
;; Produces the Point of the new position of the snake.
(define (move-head loc course)
  (cond
    [(string=? UP course)
     (make-posn (posn-x loc) (- (posn-y loc) GRID-SIZE))]
    [(string=? DOWN course)
     (make-posn (posn-x loc) (+ (posn-y loc) GRID-SIZE))]
    [(string=? LEFT course)
     (make-posn (- (posn-x loc) GRID-SIZE) (posn-y loc))]
    [(string=? RIGHT course)
     (make-posn (+ (posn-x loc) GRID-SIZE) (posn-y loc))]))


;; List-of-points Point -> List-of-points
;; Prepends new tail segment and removes the last one.
;; (check-expect (move-tail '() (make-posn 30 100)) '())
;; (check-expect (move-tail (list (make-posn 20 100)) (make-posn 30 100))
;;               (list (make-posn 30 100)))
;; (check-expect (move-tail (list (make-posn 20 100) (make-posn 20 90)) (make-posn 30 100))
;;               (list (make-posn 30 100) (make-posn 20 100)))

(define (move-tail tail head)
  (cond
    [(empty? tail) '()]
    [else (cons head (reverse (rest (reverse tail))))]))

;; (define (move-tail2 tail head)
;;   )

;; NOTE: complete this exercise at some point, I enjoy exercises like
;; Databases, etc (something real) rather than games

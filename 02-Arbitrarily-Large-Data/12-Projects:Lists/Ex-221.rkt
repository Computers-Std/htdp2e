#reader
(lib "htdp-beginner-abbr-reader.ss" "lang")
((modname Ex-221) (read-case-sensitive #t)
                  (teachpacks ())
                  (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
;; Exercises: 221 - 223

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10)
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))

(define BLOCK ; red squares with black rims
  (overlay (square (- SIZE 1) "solid" "red") (square SIZE "outline" "black")))
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)

; A Landscape is one of:
; – '()
; – (cons Block Landscape)

; A Block is a structure:
;   (make-block N N)

; interpretations: (make-block x y) depicts a block whose left corner
; is (* x SIZE) pixels from the left and (* y SIZE) pixels from the
; top
; (make-tetris b0 (list b1 b2 ...)) means b0 is the dropping
; block, while b1, b2, and ... are resting

; Data Collections (examples)
(define landscape0 (cons (make-block 2 (- HEIGHT 1)) '()))
(define landscape1
  (list (make-block 2 (- HEIGHT 1)) (make-block 4 (- HEIGHT 1)) (make-block 5 (- HEIGHT 4))))
(define block-dropping (make-block 3 4))
(define tetris0
  (make-tetris (make-block 0 0)
               (list (make-block WIDTH (- HEIGHT 1)) (make-block (- WIDTH 1) (- HEIGHT 1)))))
(define tetris0-drop (make-tetris (make-block 2 4) landscape0))
(define block-landed (make-block 1 (- HEIGHT 1)))
(define block-on-block (make-block 1 (- HEIGHT 2)))
(define tetris-b-on-b (make-tetris block-on-block (list block-landed)))
(define defualt-tetris (make-tetris (make-block (add1 (random 8)) 0) '()))

; render-tetris
; Tetris -> Image
; Block Landscape -> Image
(define (render-tetris t)
  (place-image BLOCK
               (con-block (block-x (tetris-block t)))
               (con-block (block-y (tetris-block t)))
               (render-landscape (tetris-landscape t) SCENE)))

; Landscape Image -> Image
(define (render-landscape ls bg)
  (cond
    [(empty? ls) bg]
    [else
     (place-image BLOCK
                  (con-block (block-x (first ls)))
                  (con-block (block-y (first ls)))
                  (render-landscape (rest ls) bg))]))

; Number -> Number
; converts block-x, block-y to the respective coordinates
(check-expect (con-block 9) 90)
(define (con-block n)
  (* SIZE n))

; on-tick -> tick-handler
; to-draw -> render-tetris
; on-key -> key-handler
; stop-when

; tick-handler
; Tetris -> Tetris
(check-expect (tick-handler tetris0-drop) (make-tetris (make-block 2 5) (list (make-block 2 9))))
(check-expect (tick-handler (make-tetris (make-block 1 6) (list (make-block 1 8) (make-block 1 9))))
              (make-tetris (make-block 1 7) (list (make-block 1 8) (make-block 1 9))))
(check-random (tick-handler (make-tetris (make-block 1 7) (list (make-block 1 8) (make-block 1 9))))
              (make-tetris (make-block (add1 (random 8)) 0)
                           (list (make-block 1 7) (make-block 1 8) (make-block 1 9))))
(define (tick-handler t)
  (if (and (< (block-y (tetris-block t)) (- HEIGHT 1))
           (not (member? (move-block (tetris-block t)) (tetris-landscape t))))
      (make-tetris (move-block (tetris-block t)) (tetris-landscape t))
      (make-tetris (make-block (add1 (random 8)) 0)
                   (grow-landscape (tetris-block t) (tetris-landscape t)))))

; move-block
; Block -> Block
(check-expect (move-block (make-block 2 3)) (make-block 2 4))
(define (move-block b)
  (make-block (block-x b) (add1 (block-y b))))

; Block Landscape -> Landscape
(check-expect
 (grow-landscape (make-block 2 3)
                 (list (make-block WIDTH (- HEIGHT 1)) (make-block (- WIDTH 1) (- HEIGHT 1))))
 (list (make-block 2 3) (make-block WIDTH (- HEIGHT 1)) (make-block (- WIDTH 1) (- HEIGHT 1))))
(define (grow-landscape b ls)
  (cons b ls))

; stop-when handler
; Tetris -> Boolean
(check-expect (end? (make-tetris (make-block 2 3) (list (make-block 2 8) (make-block 2 9)))) #false)
(check-expect (end? (make-tetris (make-block 2 3) (list (make-block 2 0) (make-block 2 9)))) #true)
(define (end? t)
  (if (and (not (empty? (tetris-landscape t))) (<= (block-y (first (tetris-landscape t))) 1))
      #true
      #false))

; Final Scene
; Tetris Image -> Image
(define (final-scene t)
  (overlay/align "center" "center" (text "Game Over" 15 "orange") (render-tetris t)))

; key-handler
; Tetris String -> Tetris
(check-expect (key-hand2 (make-tetris (make-block 2 0) '()) "left")
              (make-tetris (make-block 1 0) '()))
(check-expect (key-hand2 tetris0-drop "left") (make-tetris (make-block 1 4) landscape0))
(check-expect (key-hand2 tetris0-drop "right") (make-tetris (make-block 3 4) landscape0))
(define (key-hand2 t key)
  (make-tetris (make-block (cond
                             [(and (key=? key "left") (not (= (block-x (tetris-block t)) 1)))
                              (- (block-x (tetris-block t)) 1)]
                             [(and (key=? key "right") (not (= (block-x (tetris-block t)) 9)))
                              (+ (block-x (tetris-block t)) 1)]
                             [else (block-x (tetris-block t))])
                           (block-y (tetris-block t)))
               (tetris-landscape t)))

; Main
(define (main t0)
  (big-bang t0
            [to-draw render-tetris]
            [on-tick tick-handler 0.2]
            [on-key key-hand2]
            [stop-when end? final-scene]))

;; (main defualt-tetris)

#lang htdp/isl+

(require 2htdp/abstraction)

;; (for/list ((i 2) (j '(a b)))
;;   (list i j))

;; (for*/list ((i 2) (j '(a b)))
;;   (list i j))

(define width 2)

;; (for/list ([width 3] [height width])
;;   (list width height))

(for*/list ([width 3] [height width])
  (list width height))

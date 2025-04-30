#lang htdp/isl+

(lambda (x y)
  (+ x (x y)))


(lambda (x y)
  (+ x
     ((lambda (x)
        (+ (* 3 x)
           (/ 1 x)))
      (* y y))))

#lang htdp/isl+

(define (insertion-sort alon)
  (local (
          (define (sort alon)
            (cond                                            ; |
              [(empty? alon) '()]                            ; | Scope
              [else (add (first alon) (sort (rest alon)))])) ; | Box
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(> an (first alon)) (cons an alon)]
                      [else (cons (first alon)
                                  (add an (rest alon)))])])))
    (sort alon)))

(define (isort alon)
  (local ((define (sort alon)
            (cond                                            ; |
              [(empty? alon) '()]                            ; | Scope
              [else (add (first alon) (sort (rest alon)))])) ; | Box
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])])))
    (sort alon)))

; Ans: These two functions do not differ other than in name, as the
; end expression is defined inside the local scope

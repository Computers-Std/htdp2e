#lang htdp/isl+

; [List-of String] -> [List-of String]
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick (non-same names (arrangements names))))

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(check-expect (arrangements '()) (list '()))
(check-expect (arrangements '("usha" "kiran" "pallavi"))
              '(("usha" "kiran" "pallavi")
                ("kiran" "usha" "pallavi")
                ("kiran" "pallavi" "usha")
                ("usha" "pallavi" "kiran")
                ("pallavi" "usha" "kiran")
                ("pallavi" "kiran" "usha")))
(define (arrangements names)
  (cond
    [(empty? names) (list '())]
    [else
     (local
         ; String [List-of String] [List-of String] -> [List-of [List-of String]]
         ((define (insert-name-everywhere name pre post)
            (cond
              [(empty? post) (list (append pre (list name)))]
              [else
               (append (list (append pre (list name) post))
                       (insert-name-everywhere
                        name (append pre (list (first post))) (rest post)))]))
          ; String [List-of [List-of String]] -> [List-of [List-of String]]
          (define (insert-for-all-names name lon)
            (cond
              [(empty? lon) '()]
              [else
               (append (insert-name-everywhere name '() (first lon))
                       (insert-for-all-names name (rest lon)))])))
       (insert-for-all-names (first names) (arrangements (rest names))))]))

; [NEList-of X] -> X
; returns a random item from the list
(define (random-pick l)
  (local ; pick nth item in list like list-ref
      ((define (pick l n)
         (cond
           [(= n 0) (first l)]
           [else (pick (rest l) (sub1 n))]))
       (define rand-nth (random (length l))))
    (pick l rand-nth)))

; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in ll that do not agree with names at any place
(check-expect (non-same '("usha" "kiran" "pallavi")
                        (arrangements '("usha" "kiran" "pallavi")))
              '(("kiran" "pallavi" "usha") ("pallavi" "usha" "kiran")))
(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [else
     (local ((define (are-same? names lon)
               (cond
                 [(empty? lon) #false]
                 [else (or (string=? (first names) (first lon))
                           (are-same? (rest names) (rest lon)))])))
       (append
        (if (are-same? names (first ll))
            '()
            (list (first ll)))
        (non-same names (rest ll))))]))

;; Run
(gift-pick '("Louise" "Jane" "Laura" "Dana" "Mary"))

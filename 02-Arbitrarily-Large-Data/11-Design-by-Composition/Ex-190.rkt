#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 190. Design the prefixes function, which consumes a list
;; of 1Strings and produces the list of all prefixes. A list p is a
;; prefix of l if p and l are the same up through all items in p. For
;; example, (list "a" "b" "c") is a prefix of itself and (list "a" "b"
;; "c" "d").

; List-of-1Strings -> List-of-1Strings
; consumes a list of 1Strings and produces the list of all prefixes
(check-expect (prefixes (list 2 4 5 3 5 89))
              (list (list 2 4 5 3 5 89)
                    (list 2 4 5 3 5)
                    (list 2 4 5 3)
                    (list 2 4 5)
                    (list 2 4)
                    (list 2)))
(define (prefixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (append (list lo1s)
                  (prefixes (rm-last lo1s)))]))

; List-of-1Strings -> List-of-1Strings
; removes the last elment of the list
(check-expect (rm-last (list 2 4 5 3 5 89)) (list 2 4 5 3 5))
(define (rm-last lo1s)
  (cond
    [(or (empty? lo1s) (empty? (rest lo1s))) '()]
    [else (cons (first lo1s) (rm-last (rest lo1s)))]))

; List-of-1Strings -> List-of-1Strings
; consumes a list of 1Strings and produces the list of all suffixes
(check-expect (suffixes (list 2 4 5 3 5 89))
              (list (list 2 4 5 3 5 89)
                    (list 4 5 3 5 89)
                    (list 5 3 5 89)
                    (list 3 5 89)
                    (list 5 89)
                    (list 89)))
(define (suffixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (append (list lo1s)
                  (suffixes (rest lo1s)))]))

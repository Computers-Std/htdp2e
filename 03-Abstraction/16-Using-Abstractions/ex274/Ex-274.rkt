#lang htdp/isl

;; Exercise 274. Use existing abstractions to define the prefixes and
;; suffixes functions from exercise 190. Ensure that they pass the
;; same tests as the original function.

; [List-of 1String] -> [List-of [List-of 1String]]
; Produces a list of all prefixes of the given list.
(check-expect (prefixes '(1 2 3)) (list (list 1 2 3) (list 1 2) (list 1)))
(check-expect (prefixes '("a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(define (prefixes l)
  (local ((define length-ls (length l))
          (define (builder i)
            (local ((define (extractor j)
                      (list-ref l j)))
              (build-list (- length-ls i) extractor))))
    (build-list length-ls builder)))

; [List-of 1String] -> [List-of [List-of 1String]]
; For a list of 1Strings, return all the possible suffixes
(check-expect (suffixes '(2 4 5)) '((2 4 5) (4 5) (5)))
(check-expect (suffixes '(1 2 3)) '((1 2 3) (2 3) (3)))
(define (suffixes l)
  (local ((define length-l (length l))
          (define (builder i)
            (local ((define (extractor j)
                      (list-ref l (+ i j))))
              (build-list (- length-l i) extractor))))
    (build-list length-l builder)))

; [List-of 1String] -> [List-of [List-of 1String]]
; For a list of 1Strings, return all the possible prefixes
(check-expect (prefixes2 '(1 2 3)) '((1) (1 2) (1 2 3)))
(check-expect (prefixes2 '("a" "b" "c")) '(("a") ("a" "b") ("a" "b" "c")))
(define (prefixes2 lo1s)
  (local ; 1String [List-of [List-of 1String]] -> [List-of [List-of 1String]]
      ; Adds a new list with item, and prepends item to all
      ; existing lists
      ((define (merge item acc)
         (local ; [List-of 1String] -> [List-of 1String]
             ; Adds item to the beginning of list
             ((define (prepend-item lst)
                (cons item lst)))
           (map prepend-item (cons '() acc)))))
    (foldr merge '() lo1s)))

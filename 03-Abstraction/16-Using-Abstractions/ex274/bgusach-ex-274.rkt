#lang htdp/isl

;; From : https://github.com/bgusach/exercises-htdp2e/blob/master/3-abstraction/ex-274.rkt

; [List-of 1String] -> [[List-of 1String]]
; For a list of 1Strings, return all the possible prefixes
(check-expect (prefixes (list 1 2 3))
              '((1) (1 2) (1 2 3)))

(define (prefixes lo1s)
  (local (; 1String [[List-of 1String]] -> [[List-of 1String]]
          ; Adds a new list with item, and prepends item to
          ; all existing lists
          (define (merge item acc)
            (local (; [List-of 1String] -> [List-of 1String]
                    ; Adds item to the beginning of list
                    (define (prepend-item lst)
                      (cons item lst)))
              ; -- IN --
              (map prepend-item (cons '() acc)))))
    ; -- IN --
    (foldr merge '() lo1s)))

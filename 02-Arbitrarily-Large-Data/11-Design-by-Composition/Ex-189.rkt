#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ex-189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

;; Exercise 189. Here is the function search:

; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

;; It determines whether some number occurs in a list of numbers. The
;; function may have to traverse the entire list to find out that the
;; number of interest isn’t contained in the list.

;; Develop the function search-sorted, which determines whether a
;; number occurs in a sorted list of numbers. The function must take
;; advantage of the fact that the list is sorted.


; slon : Sorted-List-of-numbers

; Number List-of-numbers -> List-of-numbers
; Search Number (n) in a sorted List-of-numbers
(check-expect (search-sorted 2 (list 1 2 8 45 200)) #true)
(check-expect (search-sorted 20 (list 1 2 8 45 200)) #false)
(check-expect (search-sorted 1 (list 1 2 8 45 200)) #true)
(define (search-sorted n slon)
  (cond
    [(empty? slon) #false]
    [else (if (< n (first slon))
              #false
              (or (= n (first slon))
                  (search-sorted n (rest slon))))]))

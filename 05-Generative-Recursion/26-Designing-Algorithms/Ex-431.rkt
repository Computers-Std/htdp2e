#lang htdp/isl+

;; Exercise 431. Answer the four key questions for the bundle problem
;; and the first three questions for the quick-sort< problem. How many
;; instances of generate-problem are needed?

;; NOTE The problems of sorting an empty list or a one-item list are
;; trivially solvable. A list with many items is a nontrivial problem

;; For `bundle`
;; ===========
;; Q1) What is a trivially solvable problem?

; The trivially solvable problem is when given list is empty? or when
; the given number is zero?

;; Q2) How are trivial problems solved?

; The obvious solution is to produle an empty list '()

;; Q3) How does the algorithm generate new problems that are more
;; easily solvable than the original one? Is there one new problem
;; that we generate or are there several?

; The algorithm gernerate two new problems to act on the given list,
; one is to take the required date piece of data from the list, and
; the other is to drop that piece of data and make a new list out of
; the original list.

;; Q4) Is the solution of the given problem the same as the solution
;; of (one of) the new problems? Or, do we need to combine the
;; solutions to create a solution for the original problem? And, if
;; so, do we need anything from the original problem data?

; None of the generated problems' solution is similar to the original
; problem. But, we need to combine the solutions to create a solution
; for the original problem. And for that we need Number(n) from the
; original problem data to retrieve the data from the newly generated
; functions.

;; For `quick-sort<`
;; ===========
;; Q1) What is a trivially solvable problem?

; Sorting an empty list

;; Q2) How are trivial problems solved?

; Produce '()

;; Q3) How does the algorithm generate new problems that are more
;; easily solvable than the original one? Is there one new problem
;; that we generate or are there several?

; The algorithm generate two new problems, one for retrieving numbers
; larger than pivot(first) and other for retrieving numbers for
; smaller than pivot.

;; Q4) Is the solution of the given problem the same as the solution
;; of (one of) the new problems? Or, do we need to combine the
;; solutions to create a solution for the original problem? And, if
;; so, do we need anything from the original problem data?

; None of the solutions of newly generated problems are similar to
; original problems. But, we need to combine the solutions plus the
; first(pivot) that we passed in as argument for the new functions.

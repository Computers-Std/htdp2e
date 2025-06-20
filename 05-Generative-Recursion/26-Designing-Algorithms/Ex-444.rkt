#lang htdp/isl+

#| Exercise 444. Exercise 443 means that the design for gcd-structural
calls for some planning and a design-by-composition approach.

The very explanation of “greatest common denominator” suggests a
two-stage approach. First design a function that can compute the list
of divisors of a natural number. Second, design a function that picks
the largest common number in the list of divisors of n and the list of
divisors of m. The overall function would look like this: |#

(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))

; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k
(define (divisors k l)
  '())

; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l
(define (largest-common k l)
  1)

#| Why do you think divisors consumes two numbers? Why does it consume
S as the first argument in both uses?  |#

; Answer 1 :: The (divisors A B) function consumes two numbers, one to
; act as dividend and the other as divisor, the divisor iterates until
; 1

; Answer 2 :: The (divisors S S) function consumes S as the first
; argument, becuase the S is smallest of the two numbers, And it is
; used as a divisor for the other(larger) number.

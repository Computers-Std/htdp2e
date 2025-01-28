#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-178) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;; Exercise 178. Explain why the template for editor-kh deals with
;; "\t" and "\r" before it checks for strings of length 1

(define (editor-kh ed k)
  (cond
    [(key=? k "left") ...]
    [(key=? k "right") ...]
    [(key=? k "\b") ...]
    [(key=? k "\t") ...]
    [(key=? k "\r") ...]
    [(= (string-length k) 1) ...]
    [else ...]))

;; Answer: The reason editor-kh deals with '\t' and '\r' before the 1Strings is
; we have no use case to '\t' key, and as we (else ...) expression,
; every possible key other than the mentioned keys will come into
; place.

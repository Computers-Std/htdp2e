#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-243) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket

(define (f x) x)

(cons f '()) ;=> (list f)

(f f) ;=> f

(cons f (cons 10 (cons (f 10) '()))) ; => (list f 10 10)

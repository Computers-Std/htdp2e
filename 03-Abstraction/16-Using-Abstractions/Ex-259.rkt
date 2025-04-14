#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ex-259) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; For
;; DrRacket
(require 2htdp/batch-io)

;; Variables
(define LOC "/usr/share/dict/words")
(define DICTIONARY (read-lines LOC))

;; Data Definitions

; A Word is one of:
; '()
; (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A Low is one of:
; '()
; (cons Word List-of-words)

; String -> [List-of String]
;; Given a word, find all words that are made up from the same letters
(define (alt-words s)
  (local (
          ; Word -> String
          (define (word->string w)
            (cond
              [(empty? w) ""]
              [else (string-append (first w)
                                   (word->string (rest w)))]))
          ; [List-of Word] -> [List-of String]
          (define (words->strings low)
            (cond
              [(empty? low) '()]
              [else (cons (word->string (first low))
                          (words->strings (rest low)))]))
          ; String -> Word
          (define (string->word s)
            (explode s))
          ; [List-of String] -> [List-of String]
          ; picks those String that occur in dictionary
          (define (in-dictionary los)
            (cond
              [(empty? los) '()]
              [else (if (member? (first los) DICTIONARY)
                        (cons (first los) (in-dictionary (rest los)))
                        (in-dictionary (rest los)))]))
          ; 1String Word Word -> [List-of Word]
          ; insert-everywhere
          ; inserts 1String in every possible place of Pre and Post
          (define (in-ew 1s pre post)
            (cond
              [(empty? post) (list (append pre (list 1s)))]
              [else (append (list (append pre (list 1s) post))
                            (in-ew 1s (append pre (list (first post))) (rest post)))]))
          ; 1String [List-of Word] -> [List-of Word]
          ; like in-ew for a List-of-words
          ; insert-everywhere-in-all-words
          (define (in-ew-iaw 1s low)
            (cond
              [(empty? low) '()]
              [else (append (in-ew 1s '() (first low))
                            (in-ew-iaw 1s (rest low)))]))
          ; Word -> [List-of Word]
          ; supports only 3 letter words
          (define (arrangements word)
            (in-ew-iaw (first word) (list (rest word) (reverse (rest word))))))
    (in-dictionary (words->strings (arrangements (string->word s))))))

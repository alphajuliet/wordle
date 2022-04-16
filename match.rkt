#lang racket
;; Wordle filtering and matching
;; 2022-03-18

(provide (all-defined-out))

(require threading
         "./stats.rkt")

;;----------------
(define (string-first s)
  (if (= 0 (string-length s))
      #f
      (substring s 0 1)))

;;----------------
(define contains
  (λ~> my-string-split
       (map (curry format "(?=.*~a)") _)
       (append '(".+"))
       (string-join "")
       pregexp))

(define !contains
  ;; Does not contain these characters
  (λ~>> (format "[^~a]{5}")
        pregexp))

(define (re . elts)
  ;; Create a regexp concatenated from a list of strings
  ;; e.g. (re x ".." y "s")
  ;; re :: [String] -> RegExp
  (~> elts
      (string-join "")
      pregexp))

;;----------------
(define (filter-words wordlist pattern)
  ;; filter-words :: [String] -> RegExp -> [String]
  (filter (curry regexp-match pattern) wordlist))

(define (filter-on wordlist . patterns)
  ;; Apply consecutive patterns to the wordlist from left to right
  ;; e.g. (filter-on wh (contains "at") (!contains "rsel") "....y")
  ;; filter-on :: [String] -> RegExp ... RegExp -> [String]
  (~>> patterns
       (foldl
        (λ (pattern acc)
          (filter (curry regexp-match pattern) acc))
        wordlist)))

(define list-subtract set-subtract)

;;----------------
;; Pre-load word list
(define w (read-words "popular-words5.txt"))
(define all (read-words "words5.txt"))

;; Historical words
;; From https://eagerterrier.github.io/previous-wordle-words/alphabetical.txt
(define h (read-words "wordle-history.txt"))
(define wh (list-subtract w h))

;; The End

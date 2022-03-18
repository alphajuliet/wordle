#lang racket
;; Wordle filtering and matching
;; 2022-03-18

(provide filter-words
         filter-and)

(require threading
         rakeda
         "./stats.rkt")

;;----------------
(define (string-first s)
  (if (= 0 (string-length s))
      #f
      (substring s 0 1)))

;;----------------
(define (contains chars)
  (~>> chars
       my-string-split
       (map (λ (ch) (format "(?=.*~a)" ch)))
       (append _ '(".+"))
       (string-join _ "")
       pregexp))

(define (!contains str)
  ;; Does not contain these characters
  (~>> str
       (format "[^~a]{5}")
       pregexp))

(define (re . elts)
  ;; Create a regexp concatenated from a list of strings
  ;; e.g. (re x ".." y "s")
  ;; re :: [String] -> RegExp
  (pregexp (string-join elts "")))

;;----------------
(define (filter-words wordlist pattern)
  ;; filter-words :: [String] -> RegExp -> [String]
  (filter (curry regexp-match pattern) wordlist))

(define (filter-and wordlist . patterns)
  ;; Apply consecutive patterns to the wordlist from left to right
  ;; e.g. (filter-on wh (contains "at") (!contains "rsel") "....y")
  ;; filter-and :: [String] -> RegExp ... RegExp -> [String]
  (foldl (λ (pattern acc)
           (filter (curry regexp-match pattern) acc))
         wordlist
         patterns))

(define (remove-words v w)
  ;; Remove words in v from w
  ;; remove-words :: [String] -> [String] -> [String]
  (let ([vs (list->set v)]
        [ws (list->set w)])
    (set->list (set-subtract ws vs))))

;;----------------
;; Pre-load word list
(define w (read-words "popular-words5.txt"))
(define all (read-words "words5.txt"))

;; Historical words
;; From https://eagerterrier.github.io/previous-wordle-words/alphabetical.txt
(define h (read-words "wordle-history.txt"))
(define wh (remove-words h w))

;; The End

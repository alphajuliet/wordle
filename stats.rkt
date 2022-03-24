#lang racket
;; Wordle scoring and ranking
;; 2022-02

(provide my-string-split
         read-words
         rank-words)

(require threading
         (only-in qi define-flow ☯)
         racket/hash
         rakeda)

;;----------------
;; Utilities

(define-flow my-string-split
  ;; my-string-split :: String -> List String
  (~> (string-split "")
      rest
      (drop-right 1)))

(define-flow create-hash
  ;; create-hash :: (List a) -> (List b) -> (Hash a b)
  (~>> (map list)
       flatten
       (apply hash)))

(define (hash-add . hs)
; Add the values in two hashes where the keys match. For unmatched keys, include the value.
;   (hash-add (hash 'a 1 'b 2 'c 3) (hash 'a 4 'b 5)) => (hash 'a 5 'b 7 'c 3)
; hash-add :: Hash a Number -> Hash a Number -> ... -> Hash a Number
  (apply hash-union hs #:combine/key (λ (k v1 v2) (+ v1 v2))))

;;----------------
(define (read-words [fname "words5.txt"])
  ;; Read a file of words into a list
  ;; read-words :: IO String -> [String]
  (with-input-from-file fname
    (λ () (for/list ([line (in-lines)])
            line))))

(define-flow string-transpose
  ;; Swap axes in a list of strings.
  ;; e.g. (string-transpose '("abc" "def")) => '("ad" "be" "cf")
  ;; string-transpose :: [String] -> [String]
  (~>> (map my-string-split)
       (apply map list)
       (map (r/flip string-join ""))))

(define-flow sort-by-key
  ;; Sort by ascending key
  ;; sort-by-key :: Hash a b -> List (Pair a b)
  (~> hash->list
      (sort string<? #:key car)))

(define-flow sort-by-value
  ;; Sort by descending value
  ;; sort-by-value :: Hash a b -> List (Pair a b)
  (~> hash->list
      (sort > #:key cdr)))

;;----------------
(define (count-letters word)
  ;; Count each letter in a word
  ;; count-letters :: String -> Hash String Integer
  (foldl (λ (letter h)
           (hash-add h (hash letter 1)))
         (hash)
         (my-string-split word)))

(define (count-letters-wordlist wordlist)
  ;; Rank count of letters in decreasing order across all the words
  ;; count-letters-wordlist :: [String] -> Hash String Integer
  (foldl (λ (word h)
           (hash-add h (count-letters word)))
         (hash)
         wordlist))

(define-flow rank-by-position
  ;; rank-by-position :: [String] -> List (Hash String Integer)
  (~>> string-transpose
       (map count-letters)))

;;----------------
(define (score ranking pos letter)
  ;; Score a letter in a given position
  ;; score :: List (Hash String Integer) -> Integer -> String -> Integer
  (hash-ref (list-ref ranking pos) letter))

(define/curry (score-word ranking word)
  ;; Score a word by rank at each position as per the given matrix
  ;; score-word List (Hash String Integer) -> String -> Integer
  (apply + (for/list ([pos (in-range 5)]
                      [letter (my-string-split word)])
             (score ranking pos letter))))

(define (rank-words wordlist)
  ;; rank-words :: [String] -> List (Pair String Integer)
  (let ([r (rank-by-position wordlist)])
    (~>> wordlist
         (map (score-word r))
         (create-hash wordlist)
         sort-by-value)))

;;----------------
;; Unit tests

(module+ test

  (require rackunit
           rackunit/text-ui)

  (define-test-suite stats-tests

    (test-case "Simple tests"
      (check-equal? (my-string-split "abc") '("a" "b" "c"))
      (check-equal? (string-transpose '("abc" "def")) '("ad" "be" "cf"))
      (check-equal? (hash "a" 1 "b" 2 "c" 3) (count-letters "cbacbc"))))

  (run-tests stats-tests))

;; The End

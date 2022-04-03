#lang racket
;; Wordle scoring and ranking
;; 2022-02

(provide my-string-split
         read-words
         rank-words)

(require racket/hash
         threading)

;;----------------
;; Utilities

(define my-string-split
  ;; my-string-split :: String -> List String
  (λ~> (string-split "")
       rest
       (drop-right 1)))

(define (create-hash k v)
  ;; create-hash :: (List a) -> (List b) -> (Hash a b)
  (~>> (map list k v)
       flatten
       (apply hash)))

(define (hash-add . hs)
  ; Add the values in two hashes where the keys match. For unmatched keys, include the value.
  ;   (hash-add (hash 'a 1 'b 2 'c 3) (hash 'a 4 'b 5)) => (hash 'a 5 'b 7 'c 3)
  ; hash-add :: Hash a Number -> Hash a Number -> ... -> Hash a Number
  (apply hash-union hs
         #:combine/key (λ (k v1 v2) (+ v1 v2))))

;;----------------
(define (read-words [fname "words5.txt"])
  ;; Read a file of words into a list
  ;; read-words :: IO String -> [String]
  (with-input-from-file fname
    (λ () (for/list ([line (in-lines)])
            line))))

(define string-transpose
  ;; Swap axes in a list of strings.
  ;; e.g. (string-transpose '("abc" "def")) => '("ad" "be" "cf")
  ;; string-transpose :: [String] -> [String]
  (λ~>> (map my-string-split)
        (apply map list)
        (map (λ (x) (string-join x "")))))

(define sort-by-key
  ;; Sort by ascending key
  ;; sort-by-key :: Hash a b -> List (Pair a b)
  (λ~> hash->list
       (sort string<? #:key car)))

(define sort-by-value
  ;; Sort by descending value
  ;; sort-by-value :: Hash a b -> List (Pair a b)
  (λ~> hash->list
       (sort > #:key cdr)))

;;----------------
(define count-letters
  ;; Count each letter in a word
  ;; count-letters :: String -> Hash String Integer
  (λ~>> my-string-split
        (foldl (λ (letter h)
                 (hash-add h (hash letter 1)))
               (hash))))

(define count-letters-wordlist
  ;; Rank count of letters in decreasing order across all the words
  ;; count-letters-wordlist :: [String] -> Hash String Integer
  (λ~>> (foldl (λ (word h)
                 (hash-add h (count-letters word)))
               (hash))))

(define rank-by-position
  ;; rank-by-position :: [String] -> List (Hash String Integer)
  (λ~>> string-transpose
        (map count-letters)))

;;----------------
(define (score ranking pos letter)
  ;; Score a letter in a given position
  ;; score :: List (Hash String Integer) -> Integer -> String -> Integer
  (hash-ref (list-ref ranking pos) letter))

(define (score-word ranking word)
  ;; Score a word by rank at each position as per the given matrix
  ;; score-word List (Hash String Integer) -> String -> Integer
  (apply + (for/list ([pos (in-range 5)]
                      [letter (my-string-split word)])
             (score ranking pos letter))))

(define (rank-words wordlist)
  ;; Rank a word list in order
  ;; rank-words :: [String] -> List (Pair String Integer)
  (let ([r (rank-by-position wordlist)])
    (~>> wordlist
         (map (curry score-word r))
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
               (check-equal? (hash "a" 1 "b" 2 "c" 3) (count-letters "cbacbc"))
               (check-equal? (create-hash '(a b c) '(1 2 3)) (hash 'a 1 'b 2 'c 3))))

  (run-tests stats-tests))

;; The End

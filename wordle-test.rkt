#lang racket

(require rackunit
         rackunit/text-ui
         "stats.rkt"
         "main.rkt")

(define-test-suite wordle-tests

  (test-case "Simple tests"
             (check-equal? (normalise 4 '(1 2 3 4)) '(1/4 1/2 3/4 1))
             (check-equal? (my-string-split "abc") '("a" "b" "c"))
             (check-equal? (string-transpose '("abc" "def")) '("ad" "be" "cf"))
             (check-equal? (hash "a" 1 "b" 2 "c" 3) (count-letters "cbacbc"))
             (check-equal? (create-hash '(a b c) '(1 2 3)) (hash 'a 1 'b 2 'c 3)))

  (test-case "Match tests"
             (check-equal? (string-first "abc") "a")
             (check-equal? (string-first "") #f)
             (check-equal? (contains "ab") #px"(?=.*a)(?=.*b).+")
             (check-equal? (!contains "ab") #px"[^ab]{5}")))

(run-tests wordle-tests)

;; The End

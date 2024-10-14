#!/usr/bin/env bb
;; From the given file, extract the set of all the letters in each word.

(ns sets
  (:require [clojure.string :as str]))

(defn letters
  [word]
  (-> word
      (str/split #"")
      set
      sort
      str/join))

(def words (str/split-lines (slurp (first *command-line-args*))))
(def lines (->> words
                (map letters)
                (map #(str %1 ": " %2) words)
                (str/join "\n")))

(println lines)

;; The End

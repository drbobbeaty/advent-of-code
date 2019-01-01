(ns advent-of-code.2017.day04
  "Fourth day's solutions for the Advent of Code 2017"
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmc]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the spreadsheet data we need to compute the checksum."
  (->> "resources/2017/input/day04.txt"
    (io/reader)
    (line-seq)))

(def sample
  "This is the example data in the puzzle."
  ["aa bb cc dd ee"
   "aa bb cc dd aa"
   "aa bb cc dd aaa"])

(def sample-2
  ""
  ["abcde fghij"
   "abcde xyz ecdab"
   "a ab abc abd abf abj"
   "iiii oiii ooii oooi oooo"
   "oiii ioii iioi iiio"])

(defn one
  "Function to find the number of valid passphrases in the puzzle input.
  We do this by looking at the frequencies of the words in the phrase."
  []
  (->> (for [pf puzzle
             :let [fc (frequencies (cs/split pf #"\s"))
                   mdc (apply max (vals fc))]]
         {:pass pf :valid (not (< 1 mdc))})
    (filter :valid)
    (count)))

(defn two
  "Function to count the valid passphrases using the anagram scheme."
  []
  (->> (for [pf puzzle
             :let [agc (->> (for [[a b] (cmc/combinations (map frequencies (cs/split pf #"\s")) 2)
                                  :when (= a b)]
                              a)
                         (count))]]
         {:pass pf :valid (not (pos? agc))})
    (filter :valid)
    (count)))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (for [pf sample-2
        :let [agc (->> (for [[a b] (cmc/combinations (map frequencies (cs/split pf #"\s")) 2)
                             :when (= a b)]
                         a)
                    (count))]]
    {:pass pf :valid (not (pos? agc))}))

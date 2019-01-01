(ns advent-of-code.2017.day02
  "Second day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int sum]]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmc]
            [clojure.string :as cs]))

(def puzzle
  "This is the spreadsheet data we need to compute the checksum."
  (->> "resources/2017/input/day02.txt"
    (io/reader)
    (line-seq)
    (map (fn [s] (map #(parse-int %) (cs/split s #"\s"))))))

(def sample
  "This is the example data in the puzzle."
  (->> ["5\t1\t9\t5"
        "7 5 3"
        "2 4 6 8"]
    (map (fn [s] (map #(parse-int %) (cs/split s #"\s"))))))

(def sample-2
  "This is the example data in the puzzle."
  (->> ["5 9 2 8"
        "9 4 7 3"
        "3 8 6 5"]
    (map (fn [s] (map #(parse-int %) (cs/split s #"\s"))))))

(defn evenly
  "Function to take a sequence of numbers, and find the first pair in the
  sequence that divides evenly, and return that division. This is used in
  part 2 of the puzzle."
  [s]
  (->> (for [p (cmc/combinations s 2)
             :let [[lo hi] (apply (juxt min max) p)]
             :when (= (quot hi lo) (/ hi lo))]
         (quot hi lo))
    (first)))

(defn one
  "Function to compute the checksum of the spreadsheet based on the algorithm
  from the puzzle."
  []
  (->> (for [r puzzle
             :let [[hi lo] (apply (juxt max min) r)]]
         (- hi lo))
    (sum)))

(defn two
  "Function to find the evenly divisible checksum of the spreadsheet."
  []
  (sum (for [r puzzle] (evenly r))))

(defn yoyo
  ""
  []
  (->> (for [r sample-2]
         (evenly r))
    (sum)))

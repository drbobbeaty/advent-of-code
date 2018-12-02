(ns advent-of-code.2018.day02
  "First day's solutions for the Advent of Code 2018"
  (require [clojure.string :as cs]
           [clojure.math.combinatorics :as cmc]))

(def puzzle
  "This is the input from the frequency changes for the puzzle."
  (-> (slurp "resources/2018/input/day02.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map cs/trim))))

(defn dupes?
  "Function to return the characters in the provided string that occur
  *exactly* the provided number of times."
  [s cnt]
  (if (string? s)
    (->> (for [[k v] (frequencies s)
               :when (= v cnt)]
           [k v])
      (map first)
      (not-empty))))

(defn misses
  "Function to look at the two supplied strings and count the differences
  between them and return that as an integer. If they are the same, this
  will return a value of 0."
  [a b]
  (if (and (string? a) (string? b))
    (->> (map vector a b)
      (filter (fn [[x y]] (not= x y)))
      (count))))

(defn one
  "Function to take a sequence of box IDs and compute the checksum of the
  entire list with the rules provided."
  [& [src]]
  (let [ids (or src puzzle)
        twos (->> (map #(dupes? % 2) ids) (remove nil?) (count))
        tres (->> (map #(dupes? % 3) ids) (remove nil?) (count))]
    {:twos twos :tres tres :check (* twos tres)}))

(defn two
  "Function to find the two IDs that are only differing by one character, and
  the display those two."
  [& [src]]
  (let [ids (or src puzzle)]
    (for [[a b] (cmc/combinations ids 2)
          :let [mc (misses a b)]
          :when (= mc 1)]
      [a b])))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]
        twos (->> (map #(dupes? % 2) src) (remove nil?) (count))
        tres (->> (map #(dupes? % 3) src) (remove nil?) (count))]
    {:twos twos :tres tres :check (* twos tres)}))

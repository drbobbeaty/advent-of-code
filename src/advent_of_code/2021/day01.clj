(ns advent-of-code.2021.day01
  "First day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split]]))

(def puzzle
  "This is the input of the masses of the modules for the ship."
  (-> (slurp "resources/2021/input/day01.txt")
      (trim)
      (split #"\n")
      (->> (map #(parse-int %)))))

(def test1
  "Test data for the first part."
  [199 200 208 210 200 207 240 269 260 263])

(defn one
  "Function to find "
  [& [coll]]
  (->> (partition 2 1 (or coll puzzle))
    (map (fn [[a b]] (< a b)))
    (filter identity)
    (count)))

(defn two
  "Function to find "
  [& [coll]]
  (->> (partition 3 1 puzzle)
    (map #(apply + %))
    (one)))

(ns advent-of-code.2020.day01
  "First day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the masses of the modules for the ship."
  (-> (slurp "resources/2020/input/day01.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map #(parse-int %)))))

(def test1
  "Test data for the first part."
  [1721 979 366 299 675 1456])

(defn one
  "Function to sum all the fuel requirements for all the modules args"
  [& coll]
  (first
    (for [[a b] (combinations (or coll puzzle) 2)
          :when (= 2020 (+ a b))]
      (* a b))))

(defn two
  "Function to sum all fuel requirements assuming the recursive formula
  where all the fuel also has mass, and we need to inlude all that mass
  and fuel in the calculation."
  [& coll]
  (first
    (for [[a b c] (combinations (or coll puzzle) 3)
          :when (= 2020 (+ a b c))]
      (* a b c))))

(ns advent-of-code.2022.day03
  "Third day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum ascii-a
                                         ascii-A ascii-z ascii-Z]]
            [clojure.set :refer [intersection]]))

(def puzzle
  "This is the input of the contents of the backpacks for the elves."
  (-> (slurp "resources/2022/input/day03.txt")
    (trim)
    (split #"\n")))

(def test1
  "Test data for the first part."
  (-> ["vJrwpWtwJgWrhcsFMMfFFhFp"
       "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
       "PmmdzqPrVvPwwTWBwg"
       "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
       "ttgJtRGJQctTZtZT"
       "CrZsJsPPZsGzwwsLwLmpwMDw"]))

(defn bisect
  "Function to bisect a collection into two halves."
  [coll]
  (partition (/ (count coll) 2) coll))

(defn over
  "Function to find the overlap between the sequences - there should
  be only one element, so take that."
  [coll]
  (->> (map set coll)
    (apply intersection)
    (first)))

(defn priority
  "Function to return the numerical value of the item provided, based on
  the rules that the elves live by."
  [c]
  (let [nc (int c)]
    (cond
      (<= ascii-a nc ascii-z) (+ (- nc ascii-a) 1)
      (<= ascii-A nc ascii-Z) (+ (- nc ascii-A) 27))))

(defn one
  "Function to find the common element in each side of each backpack, and
  then get it's priority, and then sum all those values."
  [& [coll]]
  (->> puzzle
    (map bisect)
    (map over)
    (map priority)
    (sum)))

(defn two
  "Function to find the common badge in each group of three elves backpack
  contents, and prioritize and sum those."
  [& [coll]]
  (->> puzzle
    (partition 3)
    (map over)
    (map priority)
    (sum)))

(ns advent-of-code.2021.day06
  "Sixth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take all the lanternfish in the school, and group them by age,
  and then put them into a sequence where the index is the age of the fish.
  As they are the same age, they will travel together, and simply add together."
  [coll]
  (let [m (frequencies coll)]
    (for [i (range 9)] (get m i 0))))

(def puzzle
  "This is the input of the population age of the lanternfish outside the sub."
  (-> (slurp "resources/2021/input/day06.txt")
    (trim)
    (split #",")
    (->> (map parse-int))
    (parse)))

(def test1
  "Test data for the first part."
  (-> "3,4,3,1,2"
    (split #",")
    (->> (map parse-int))
    (parse)))

(defn day
  "Function to take a sequence of lanternfish populations, where the index
  is the age of the group, and age them by a day. All the fish that are
  0 days old, will become 6 days, and will generate a new 8-day old fish."
  [coll]
  (let [births (first coll)]
    (-> (conj (vec (rest coll)) births)
      (assoc 6 (+ births (nth coll 7 0))))))

(defn one
  "Function to find the number of lanternfish in the school after 80 days."
  [& [coll]]
  (sum (first (drop 80 (iterate day puzzle)))))

(defn two
  "Function to find the number of lanternfish in the school after 256 days."
  [& [coll]]
  (sum (first (drop 256 (iterate day puzzle)))))

(ns advent-of-code.2022.day01
  "First day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]))

(def puzzle
  "This is the input of the calories in each item each elf is carying."
  (-> (slurp "resources/2022/input/day01.txt")
    (trim)
    (split #"\n")
    (->> (map #(parse-int %))
      (partition-by #(= 0 %))
      (remove #(= [0] %)))))

(def test1
  "Test data for the first part."
  (-> ["1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "" "10000"]
    (->> (map #(parse-int %))
      (partition-by #(= 0 %))
      (remove #(= [0] %)))))

(defn one
  "Function to find the maximum number of calories any one elf is carying."
  [& [coll]]
  (apply max (map sum puzzle)))

(defn two
  "Function to find total claories carried by the topw three elves."
  [& [coll]]
  (->> puzzle
    (map sum)
    (sort)
    (reverse)
    (take 3)
    (sum)))

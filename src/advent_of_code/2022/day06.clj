(ns advent-of-code.2022.day06
  "Sixth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]))

(def puzzle
  "This is the input of the signal stream from the device."
  (-> (slurp "resources/2022/input/day06.txt")
    (trim)))

(def test1
  "Test data for the first part."
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
   "bvwbjplbgvbhsrlpgdmjqwftvncz"
   "nppdvjthqldpwncqszvftbrmjlhg"
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn sop
  "Function to return the end of the start-of-packet in the signal."
  [s]
  (first
    (for [[i fr] (map vector (range) (partition 4 1 s))
          :when (= 4 (count (distinct fr)))]
      (+ i 4))))

(defn one
  "Function to find the end of the start-of-packet for the signal."
  [& [coll]]
  (sop puzzle))

(defn som
  "Function to return the end of the start-of-message in the signal."
  [s]
  (first
    (for [[i fr] (map vector (range) (partition 14 1 s))
          :when (= 14 (count (distinct fr)))]
      (+ i 14))))

(defn two
  "Function to find the end of the start-of-message for the signal."
  [& [coll]]
  (som puzzle))

(ns advent-of-code.2022.day04
  "Fourth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]))

(defn parse
  "Function to take the line from the puzzle input and make it into the
  necessary two tuples for easier processing."
  [arg]
  (let [[xl xh yl yh] (rest (map parse-int (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" arg)))]
    [[xl xh] [yl yh]]))

(def puzzle
  "This is the input of the seection assignments for the elves."
  (-> (slurp "resources/2022/input/day04.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["2-4,6-8"
       "2-3,4-5"
       "5-7,7-9"
       "2-8,3-7"
       "6-6,4-6"
       "2-6,4-8"]
    (->> (map parse))))

(defn within?
  "Predicate function to see if the first tuple completely fits within the
  second."
  [[al ah] [bl bh]]
  (<= bl al ah bh))

(defn one
  "Function to find the number of completely duplicate elf assignments."
  [& [coll]]
  (->> puzzle
    (filter (fn [[a b]] (or (within? a b) (within? b a))))
    (count)))

(defn overlap?
  "Predicate function to see if the first tuple pverlaps the second - even a
  bit."
  [[al ah] [bl bh]]
  (and (<= bl ah) (<= al bh)))

(defn two
  "Function to find the number of overlapping elf assignments."
  [& [coll]]
  (->> puzzle
    (filter (fn [[a b]] (or (overlap? a b) (overlap? b a))))
    (count)))

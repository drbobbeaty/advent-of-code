(ns advent-of-code.2016.day18
  "Eighteenth day's solutions for the Advent of Code 2016"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn next-row
  "Function to take an existing row map and generate the next row based on the
  rules of the puzzle."
  [s]
  (apply str
    (for [x (partition 3 1 (str "." s "."))]
      (case (apply str x)
        ("^^." ".^^" "^.." "..^") \^
        \.))))

(defn safes
  "Function to take a sequence of rows of floor tiles, and counts all the safe
  ones."
  [s]
  (count (filter #(= \. %) s)))

(def puzzle
  "This is the first row of the floor - the start of the puzzle."
  "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^.")

(defn one
  "Function to compute 40 rows of the floor, and then count the safe tiles
  in the floor."
  []
  (let [floor (take 40 (iterate next-row puzzle))]
    (apply + (map safes floor))))

(defn two
  "Function to compute 400000 rows of the floor, and then count the safe tiles
  in the floor."
  []
  (let [floor (take 400000 (iterate next-row puzzle))]
    (apply + (map safes floor))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [floor (take 10 (iterate next-row ".^^.^.^^^^"))]
    (apply + (map safes floor))))

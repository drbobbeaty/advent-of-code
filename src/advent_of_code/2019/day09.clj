(ns advent-of-code.2019.day09
  "Ninth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the BOOST program for my Intcode computer."
  (-> (slurp "resources/2019/input/day09.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-int))))

(defn yoyo
  "Run the test inputs for the final additions to the Intcode computer"
  []
  (for [mems [[109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
              [1102 34915192 34915192 7 4 7 99 0]
              [104 1125899906842624 99]]]
    (select-keys (run mems) [:output])))

(defn one
  "Function to run the BOOST program in debug mode and get the BOOST keycode
  for validation that all opcodes and addressing modes are working."
  []
  (select-keys (run puzzle 1) [:output]))

(defn two
  "Function to run the BOOST program in sensor boost mode, and get the
  coordinates of Santa."
  []
  (select-keys (run puzzle 2) [:output]))

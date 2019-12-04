(ns advent-of-code.2019.day04
  "Fourth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(defn valid?
  "Predicate function to check if the password matches the rules: 6 digits,
  always increasing and at least two duplicated digits."
  [x]
  (let [sx (str x)
        len (count sx)]
    (and (= 6 len)
         (= (seq sx) (sort sx))
         (not= len (count (distinct sx))))))

(defn stricter?
  "Predicate functoin to check if the password matches the rules: 6 digits,
  always increasing and at least two duplicated digits - but also that the
  duplicated digits are exactly two, and no more. But only one such pair
  needs to exist in the number."
  [x]
  (let [sx (str x)]
    (and (= 6 (count sx))
         (= (seq sx) (sort sx))
         (<= 0 (.indexOf (vals (frequencies sx)) 2)))))

(defn one
  "Function to find, and count, all the valid passwords in the range I've
  been given. This just uses the validator function and rips through all
  the possible values."
  []
  (count (filter valid? (range 125730 (inc 579381)))))

(defn two
  "Function to find, and count, all the valid passwords in the range I've
  been given. This uses the stricter password validator for the second
  part of the puzzle."
  []
  (count (filter stricter? (range 125730 (inc 579381)))))


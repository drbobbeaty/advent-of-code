(ns advent-of-code.2020.day06
  "Sixth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.2020.day04 :refer [collect]]
            [advent-of-code.util :refer [trim split sum]]
            [clojure.set :refer [intersection]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the groups of answers from each group of passengers.
  the different people in the group are separated by a space to make it clear
  for part 2."
  (-> (slurp "resources/2020/input/day06.txt")
      (trim)
      (split #"\n")
      (collect)))

(def test1
  "Test data for the first part."
  (-> ["abc"
       ""
       "a"
       "b"
       "c"
       ""
       "ab"
       "ac"
       ""
       "a"
       "a"
       "a"
       "a"
       ""
       "b"]
      (collect)))

(defn one
  "Function to look at all the 'yes' answers in a group, collected by the
  'colelct' function in the parsing of the input data, and get the unique
  'yes' answers in each group, and then sum them all up."
  [& [coll]]
  (sum
    (for [ys (or coll puzzle)]
      (count (distinct (cs/replace ys " " ""))))))

(defn two
  "Function to count the *common* 'yes' answers by each member of the group,
  and then sum up only those. This means we had to refactor the parsing logic
  to make it simpler, and leave in the spaces between passengers, so that we
  could use that here, to make the individual sets for a passwnger, and then
  get the intersection of all the people in each group."
  [& [coll]]
  (sum
    (for [ys (or coll puzzle)
          :let [yss (map set (split ys " "))]]
      (count (apply intersection yss)))))

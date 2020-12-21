(ns advent-of-code.2020.day21
  "Twenty-first day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split un-seq]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse a line of the ingredients from the puzzle input."
  [s]
  (let [[ing bad] (rest (re-matches #"^(.+) \(contains (.+)\)$" s))]
    {:ingredients (split (trim ing) " ") :bad (split (trim bad) ", ")}))

(def puzzle
  "This is the input of the ingredients and allergens that we need to be
  careful about."
  (-> (slurp "resources/2020/input/day21.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
       "trh fvjkl sbzzf mxmxvkd (contains dairy)"
       "sqjhc fvjkl (contains soy)"
       "sqjhc mxmxvkd sbzzf (contains fish)"]
      (->> (map parse))))

(defn one
  "Function to "
  [& [coll]]
  test1
  )

(defn two
  "Function to "
  [& [coll]]
  )

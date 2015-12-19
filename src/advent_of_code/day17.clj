(ns advent-of-code.day17
  (:require [clojure.math.combinatorics :refer [subsets]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the bucket sizes."
  (for [l (cs/split-lines (slurp "resources/input/day17.txt"))]
    (Integer/parseInt l)))

(defn match
  "Function to take the data and create a sequence of vectors with the total
  size matching the argument, along with the count of containers and what those
  containers are. This is the basis of the rest of the code."
  [puz sz]
  (let [itov (fn [s] (map #(nth puz %) s))]
    (->> (range (count puzzle))
         (subsets)
         (map itov)
         (map #(vector (apply + %) (count %) %))
         (filter #(= sz (first %))))))

(defn part1
  "Function to simply count how many ways to make the proper size."
  []
  (count (match puzzle 150)))

(defn part2
  "Function to count the number of minimal counts we have."
  []
  (let [md (match puzzle 150)
        mcc (apply min (map second md))]
    (count (filter #(= mcc (second %)) md))))

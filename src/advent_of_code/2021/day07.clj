(ns advent-of-code.2021.day07
  "Sixth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median mean]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the positions of all the crab subs around me."
  (-> (slurp "resources/2021/input/day07.txt")
    (trim)
    (split #",")
    (->> (map parse-int))))

(def test1
  "Test data for the first part."
  (-> "16,1,2,0,4,2,7,1,2,14"
    (split #",")
    (->> (map parse-int))))

(defn one
  "Function to find the fuel used to get all the crab subs aligned - but
  it needs to be the minimum amount, as they don't have a lot. So we use
  the median of the positions of the crab subs, and that's the position
  where half are above and half below, and should be the target."
  [& [coll]]
  (sum (map #(abs (- (median puzzle) %)) puzzle)))

(defn two
  "Function to find the fuel used to get all the crab subs to line up,
  but this uses a different fuel consumption scheme, and so we need to
  move from the median to the mean of the positions, and then, because
  of fractions, let's look at points on either side of the mean as well."
  [& [coll]]
  (let [dist (fn [d] (let [n (abs d)] (/ (* n (inc n)) 2)))
        tgt (int (mean puzzle))]
    (for [dx [-1 0 1]]
      {:tgt (+ dx tgt) :fuel (sum (map #(dist (- (+ dx tgt) %)) puzzle))})))

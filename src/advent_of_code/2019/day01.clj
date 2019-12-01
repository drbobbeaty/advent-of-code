(ns advent-of-code.2019.day01
  "First day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the frequency changes for the puzzle."
  (-> (slurp "resources/2019/input/day01.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map #(parse-int %)))))

(defn one
  "Function to sum all the fuel requirements for all the modules args"
  [& coll]
  (apply + (map #(- (quot % 3) 2) (or coll puzzle))))

(defn two
  "Function to sum all fuel requirements assuming the recursive formula
  where all the fuel also has mass, and we need to inlude all that mass
  and fuel in the calculation."
  [& coll]
  (let [calc (fn [x] (loop [mass x
                            fuel 0]
                       (let [nf (- (quot mass 3) 2)]
                         (if (pos? nf)
                           (recur nf (+ fuel nf))
                           fuel))))]
    (apply + (map calc (or coll puzzle)))))

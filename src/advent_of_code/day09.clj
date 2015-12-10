(ns advent-of-code.day09
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the distances for Santa."
  (into {} (for [l (cs/split-lines (slurp "resources/input/day09.txt"))
                 :let [[_ src dest mi] (re-matches #"(.*?) to (.*?) = (.*?)" l)]]
             [(sort [src dest]) (Integer/parseInt mi)])))

(def cities
  "This is the list of cities that Santa has to visit"
  (distinct (flatten (keys puzzle))))

(defn distance
  "Function to compute the distance of trip as provided by a sequence of cities."
  [cs]
  (if (coll? cs)
    (apply + (for [l (partition 2 1 cs)]
               (puzzle (sort l))))))

(defn shortest
  "Function to find the shortest distance for Santa."
  [cs]
  (apply min (map distance (permutations cs))))

(defn longest
  "Function to find the longest distance for Santa."
  [cs]
  (apply max (map distance (permutations cs))))

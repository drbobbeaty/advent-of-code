(ns advent-of-code.day12
  (:require [cheshire.core :as json]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for Santa's books."
  (json/parse-string (slurp "resources/input/day12.json") true))

(defn sum
  "Function to sum all the values in the provided input - accoring to Santa's
  rules."
  [x]
  (println exclude)
  (cond
    (nil? x) 0
    (string? x) 0
    (number? x) x
    (keyword? x) 0
    (map? x) (if (empty? x) 0 (apply + (for [[k v] x] (+ (sum k) (sum v)))))
    (coll? x) (if (empty? x) 0 (apply + (map sum x)))
    :else 0))

(defn sum-red
  "Function to sum all the values in the provided input - accoring to Santa's
  rules. But this time, we exclude the maps with 'red' in the values."
  [x]
  (cond
    (nil? x) 0
    (string? x) 0
    (number? x) x
    (keyword? x) 0
    (map? x) (if (or (empty? x)
                     (<= 0 (.indexOf (vals x) "red"))) 0 (apply + (for [[k v] x] (+ (sum k) (sum v)))))
    (coll? x) (if (empty? x) 0 (apply + (map sum x)))
    :else 0))


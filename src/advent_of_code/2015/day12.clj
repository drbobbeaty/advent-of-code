(ns advent-of-code.2015.day12
  (:require [cheshire.core :as json]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for Santa's books."
  (json/parse-string (slurp "resources/2015/input/day12.json") true))

(def ^:dynamic *kill* nil)

(defn sum
  "Function to sum all the values in the provided input - accoring to Santa's
  rules."
  [x]
  (let [done? (fn [x] (or (empty? x)
                          (and *kill* (<= 0 (.indexOf (vals x) *kill*)))))]
    (cond
      (nil? x) 0
      (string? x) 0
      (number? x) x
      (keyword? x) 0
      (map? x) (if (done? x) 0 (apply + (for [[k v] x] (+ (sum k) (sum v)))))
      (coll? x) (if (empty? x) 0 (apply + (map sum x)))
      :else 0)))

(defmacro with-kill
  "Macro to set up the dynamic kill value variable for the processing."
  [kk & body]
  `(binding [*kill* ~kk]
     ~@body))

(defn sum-red
  "Function to sum all the values in the provided input - accoring to Santa's
  rules. But this time, we exclude the maps with 'red' in the values."
  [x]
  (with-kill "red"
    (sum x)))

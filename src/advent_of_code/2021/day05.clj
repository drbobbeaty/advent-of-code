(ns advent-of-code.2021.day05
  "Fifth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum compact]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to map the string representation of the lines into two tuples where
  each endpoint is an [x y] that can then me manipulated a lot easier."
  [s]
  (let [[_ a b c d] (re-matches #"^(\d+),(\d+) -> (\d+),(\d+)$" s)]
    [[(parse-int a) (parse-int b)] [(parse-int c) (parse-int d)]]))

(def puzzle
  "This is the input of the locations of the hydrothermal vent lines."
  (-> (slurp "resources/2021/input/day05.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["0,9 -> 5,9"
       "8,0 -> 0,8"
       "9,4 -> 3,4"
       "2,2 -> 2,1"
       "7,0 -> 7,4"
       "6,4 -> 2,0"
       "0,9 -> 2,9"
       "3,4 -> 1,4"
       "0,0 -> 8,8"
       "5,5 -> 8,2"]
    (->> (map parse))))

(defn manhat?
  "Predicate function to say of the line segment is manhattan - along an axis.
  This will then return false if the angle fo the line is other than 0 or 90."
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn steps
  "Function to convert a pair of endpoints to a sequence of all the points
  between the endpoints, including the endpoints. This is a way to get the
  'steps' from end to end, inclusive."
  [[[x1 y1] [x2 y2]]]
  (let [dx (if (< x1 x2) (min (- x2 x1) 1) (max (- x2 x1) -1))
        dy (if (< y1 y2) (min (- y2 y1) 1) (max (- y2 y1) -1))]
    (loop [x x1
           y y1
           ans (transient [])]
      (if (and (= x x2) (= y y2))
        (persistent! (conj! ans [x y]))
        (recur (+ x dx) (+ y dy) (conj! ans [x y]))))))

(defn crosses
  "Function to take a sequence of lines defined by endpoints, and then expands
  each to the steps for each line segment, and then count the number of points
  where two or more lines intersect."
  [coll]
  (let [pts (->> coll
              (map steps)
              (apply concat))]
    (-> (for [[k v] (frequencies pts)] (if (< 1 v) [k v]))
      (compact)
      (count))))

(defn one
  "Function to find the number of points where at least two lines of
  hydrothermal vents overlap - assuming the vents have to be manhattan
  in shape - fitting along an axis."
  [& [coll]]
  (crosses (filter manhat? puzzle)))

(defn two
  "Function to find the number of points where at least two lines of
  hydrothermal vents overlap - and these can be at 45 deg angles as well."
  [& [coll]]
  (crosses puzzle))

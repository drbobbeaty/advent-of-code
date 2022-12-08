(ns advent-of-code.2022.day08
  "Eighth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take an input line from the copter scan, and turn it into a
  sequence of tree heights for the forrest scan."
  [s]
  (map (comp parse-int str) s))

(def puzzle
  "This is the input of the "
  (-> (slurp "resources/2022/input/day08.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["30373"
       "25512"
       "65332"
       "33549"
       "35390"]
    (->> (map parse))))

(defn visible
  "Predicate function to return the visibility status of the tree at [r c]
  in the forrest scan of trees. This will short-circuit and complete as soon
  as it knows an answer, but it still has to seach to prove that."
  [trees r c]
  (let [rmax (count trees)
        cmax (count (first trees))
        height (fn [r c] (nth (nth trees r []) c -1))
        me (height r c)]
    (some pos?
      (for [[dx dy] [[0 -1] [1 0] [0 1] [-1 0]]]
        (loop [x (+ c dx)
               y (+ r dy)]
          (let [h (height y x)]
            (cond
              (<= me h)                                      0
              (or (<= x 0) (<= cmax x) (<= y 0) (<= rmax y)) 1
              :else
                (recur (+ x dx) (+ y dy)))))))))

(defn one
  "Function to find the number of trees that are clearly visible from the
  perimeter of the forrest."
  [& [coll]]
  (let [trees puzzle
        rmax (count trees)
        cmax (count (first trees))]
    (sum
      (for [r (range rmax) c (range cmax)
           :when (visible trees r c)]
        1))))

(defn views
  "Function to return the number of trees in each direction that can be
  seen from the provided location in the forrest."
  [trees r c]
  (let [rmax (count trees)
        cmax (count (first trees))
        height (fn [r c] (nth (nth trees r []) c -1))
        me (height r c)]
    (for [[dx dy] [[0 -1] [1 0] [0 1] [-1 0]]]
      (loop [x (+ c dx)
             y (+ r dy)
             cnt 0]
        (let [h (height y x)]
          (cond
            (<= me h)                                    (inc cnt)
            (or (< x 0) (<= cmax x) (< y 0) (<= rmax y)) cnt
            :else
              (recur (+ x dx) (+ y dy) (inc cnt))))))))

(defn two
  "Function to find the ideal spot for the treehouse by getting the
  maximum scenic score for the forrest."
  [& [coll]]
  (let [trees puzzle
        rmax (count trees)
        cmax (count (first trees))]
    (apply max
      (for [r (range rmax) c (range cmax)]
        (apply * (views trees r c))))))

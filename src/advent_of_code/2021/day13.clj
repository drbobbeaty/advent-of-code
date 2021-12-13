(ns advent-of-code.2021.day13
  "Thirteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take the mixed lines from the transparent page, and parse out
  the coordinates of the dots, and the locations of the folds, and return them
  to the caller in a much more useful format."
  [coll]
  (let [dots (->> coll
               (remove #(neg? (.indexOf % ",")))
               (map #(map parse-int (split % ","))))
        folds (->> coll
                (remove #(neg? (.indexOf % "=")))
                (map #(split (apply str (drop 11 %)) "="))
                (map (fn [[a b]] [a (parse-int b)])))]
    {:dots dots :folds folds}))

(def puzzle
  "This is the input of the first page of the manual for the thermal imaging
  camera for the sub."
  (-> (slurp "resources/2021/input/day13.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["6,10"
       "0,14"
       "9,10"
       "0,3"
       "10,4"
       "4,11"
       "6,0"
       "6,12"
       "4,1"
       "0,13"
       "10,12"
       "3,4"
       "3,0"
       "8,4"
       "1,10"
       "2,14"
       "8,10"
       "9,0"
       ""
       "fold along y=7"
       "fold along x=5"]
    (parse)))

(defn fold-y
  "Function to fold the paper on the y=fy line, and unique the dots so that
  it returns ready to be folded again."
  [dots fy]
  (distinct (for [[x y] dots] [x (if (< fy y) (- fy (- y fy)) y)])))

(defn fold-x
  "Function to fold the paper on the x=fx line, and unique the dots so that
  it returns ready to be folded again."
  [dots fx]
  (distinct (for [[x y] dots] [(if (< fx x) (- fx (- x fx)) x) y])))

(defn one
  "Function to find the number of unique dots on the page after the first
  fold."
  [& [coll]]
  (let [{dots :dots folds :folds} puzzle
        [ffa ffv] (first folds)]
    (count ((if (= ffa "x") fold-x fold-y) dots ffv))))

(defn show
  "Function to take a sequence of dots, and create a visual image of them
  as a sequence of strings. This is both a simple check on the data, and
  the key to solving part 2."
  [dots]
  (let [xmax (apply max (map first dots))
        ymax (apply max (map last dots))]
    (for [r (range (inc ymax))]
      (apply str
        (for [c (range (inc xmax))] (if (neg? (.indexOf dots [c r])) "." "#"))))))

(defn two
  "Function to find the eight character code that is created after you fold
  the transparent paper along all fold lines."
  [& [coll]]
  (loop [dots (:dots puzzle)
         folds (:folds puzzle)]
    (if-let [[ffa ffv] (first folds)]
      (recur ((if (= ffa "x") fold-x fold-y) dots ffv) (rest folds))
      (show dots))))

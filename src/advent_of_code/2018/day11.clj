(ns advent-of-code.2018.day11
  "Eleventh day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(def serial-number
  "This is the serial number we have to use on the pwer grid based on the
  puzzle."
  7803)

(defn power
  "Function to compute the power of a given grid cell with a provided serial
  number of the grid. This will be calculated for each of the 300x300 cells
  in the grid."
  [x y sn]
  (let [rack (+ 10 x)
        p (quot (* rack (+ sn (* y rack))) 100)]
    (- (- p (* 10 (quot p 10))) 5)))

(defn grid
  "Function to make a sequence of sequences where they are ordered row-major
  format in the sequences, so that the calculation of the regional power will
  be easy."
  [sn]
  (let [pc (fn [[x y]] (power x y sn))]
    (for [y (range 1 (inc 300))]
      (->> (map vector (range 1 (inc 300)) (repeat y))
        (map pc)))))

(defn cluster
  "Function to look at the grid of power numbers and break it into nxn
  sub-grid sections, and then sum the power for each of these sub-grids.
  The results will be a sequence of vectors:
    [[x y sz] power]
  so that you can do anything with it you want."
  [sz s]
  (let [rows (map #(map (fn [s] (apply + s)) (partition sz 1 %)) s)
        tsgs (for [rs (partition sz 1 rows)]
               (map #(apply + %) (apply map vector rs)))]
    (for [[y sgt] (map vector (range) tsgs)
          [x v] (map vector (range) sgt)]
      [[(inc x) (inc y) sz] v])))

(defn one
  "Function to find the largest 3x3 power sub-grid in the grid defined by
  the supplied serial number - or we'll use the one from the puzzle as a
  default. This will return:
    [[x y 3] power]
  so that it's clear what the top-left position is, and the total power."
  [& [sn]]
  (->> (grid (or sn serial-number))
    (cluster 3)
    (sort-by second >)
    (first)))

(defn two
  "Function to find the largest nxn power sub-grid in the grid defined by
  the supplied serial number - or we'll use the one from the puzzle as a
  default. This will return:
    [[x y n] power]
  so that it's clear what the top-left position is, the size of the sub-grid,
  and the total power for that sub-grid."
  [& [sn]]
  (let [pg (grid (or sn serial-number))]
    (->> (for [n (range 1 (inc 300))]
           (->> (cluster n pg)
             (sort-by second >)
             (first)))
      (sort-by second >)
      (first))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [pts [[3 5 8] [122 79 57] [217 196 39] [101 153 71]]]
    (for [[x y sn] pts]
      (power x y sn))))

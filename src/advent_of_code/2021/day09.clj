(ns advent-of-code.2021.day09
  "Ninth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum compact]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the cave topology around the sub."
  (-> (slurp "resources/2021/input/day09.txt")
    (trim)
    (split #"\n")
    (as-> s (map #(map parse-int (seq %)) s))))

(def test1
  "Test data for the first part."
  (-> ["2199943210"
       "3987894921"
       "9856789892"
       "8767896789"
       "9899965678"]
    (as-> s (map #(map parse-int (seq %)) s))))

(defn adjacent
  "Function to return a sequence of all the adjacent values to the provided
  row, r, and column, c. In the middle this will be 4 values, on the sides,
  it will be 3, and in the corners, it'll be just 2."
  [m r c]
  (let [rmax (dec (count m))
        cmax (dec (count (first m)))
        row (nth m r)
        w (if (< 0 c) (nth row (dec c)))
        e (if (< c cmax) (nth row (inc c)))
        n (if (< 0 r) (nth (nth m (dec r)) c))
        s (if (< r rmax) (nth (nth m (inc r)) c))]
    (compact [w e n s])))

(defn lows
  "Function to find all the local minima in the topology of the scan that
  the sub has provided us. This will return a sequence of row, col, and value
  of each of the minima, and we can work with that."
  [m]
  (let [rcnt (count m)
        ccnt (count (first m))]
    (-> (for [r (range rcnt)
              c (range ccnt)
              :let [v (nth (nth m r) c)]]
          (if (< v (apply min (adjacent m r c)))
            {:row r :col c :val v}))
      (compact))))

(defn one
  "Function to find the number of low points in the bottom topology of the
  cave. We look at the neighbord of each point, and see if the point is a
  local minima. If so, we could it."
  [& [coll]]
  (->> (lows puzzle)
    (map :val)
    (map inc)
    (sum)))

(defn flow
  "Function to start at a point, see if it is the end of the basin, and
  if not, then keep flowing out in all four directions: N, E, S, W and
  repeat - putting together the list of all the visited points, and then
  return that when the basin limits have been found."
  [m r c visit]
  (let [rmax (dec (count m))
        cmax (dec (count (first m)))
        v (nth (nth m r) c)]
    (if (= 9 v)
      visit
      (let [vs (atom (conj visit [r c v]))
            unk? (fn [ro co] (every? #(not= [ro co] (take 2 %)) @vs))]
        (doseq [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
                :let [nr (+ r dr)
                      nc (+ c dc)]
                :when (and (<= 0 nr rmax) (<= 0 nc cmax) (unk? nr nc))]
          (reset! vs (distinct (concat @vs (flow m nr nc @vs)))))
        @vs))))

(defn two
  "Function to find the three largest basins, by size, and then compute
  the product of those three basin sizes, and return that."
  [& [coll]]
  (->> (for [{r :row c :col v :val} (lows puzzle)
             :let [basin (flow puzzle r c [])]]
         {:basin basin :size (count basin)})
    (sort-by :size)
    (reverse)
    (take 3)
    (map :size)
    (apply *)))

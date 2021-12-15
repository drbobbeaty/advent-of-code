(ns advent-of-code.2021.day15
  "Fifteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a sequence of strings that are the risk map for this cave
  and turn it into a sequence of seqences for the values, and all the metadata
  on the map - the starting point, the ending point, and the limits on the map."
  [coll]
  (let [rows (count coll)
        cols (count (first coll))
        risk (map #(map parse-int (seq %)) coll)]
    {:map risk :rows rows :rmax (dec rows) :cols cols :cmax (dec cols) :start [0 0] :end [(dec rows) (dec cols)]}))

(def puzzle
  "This is the input of the chiton risk density map for the cave we are in."
  (-> (slurp "resources/2021/input/day15.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["1163751742"
       "1381373672"
       "2136511328"
       "3694931569"
       "7463417111"
       "1319128137"
       "1359912421"
       "3125421639"
       "1293138521"
       "2311944581"]
    (parse)))

(defn map-it
  "Function to use a simple walking plan to see what the least-expensive
  route to each cell is in the map, and then return the cost to get to the
  endpoint in the map - as defined by the input."
  [cmap]
  (let [risk (fn [r c] (nth (nth (:map cmap) r) c))
        hood (fn [r c] (for [[dr dc] [[1 0] [-1 0] [0 1] [0 -1]]
                             :let [nr (+ r dr)
                                   nc (+ c dc)]
                             :when (and (<= 0 nr (:rmax cmap)) (<= 0 nc (:cmax cmap)))]
                         [nr nc]))
        costs (atom {(:start cmap) 0})]
    (loop [ptc [(:start cmap)]]
      (if-let [loc (first ptc)]
        (let [cloc (get @costs loc)
              mptc (atom [])]
          (doseq [[r c] (apply hood loc)
                  :let [nrisk (risk r c)
                        tcost (get @costs [r c] :empty)]
                  :when (or (= :empty tcost) (< (+ cloc nrisk) tcost))]
            (swap! costs assoc [r c] (+ cloc nrisk))
            (swap! mptc conj [r c]))
          (recur (concat (rest ptc) @mptc)))
        (get @costs (:end cmap))))))

(defn one
  "Function to find the lowest cost of traveling through the cave based on the
  risk map, and return the total value of the risk we encountered."
  [& [coll]]
  (map-it puzzle))

(defn jumbo
  "Function to supersize the input from the puzzle to allow it to be used
  as input to the second part. This will do the 5x5 gridding, with the
  incrementing and rollovers, and then return something that looks like
  the input, but is lots bigger."
  [cmap]
  (let [risk (:map cmap)
        ans (atom [])
        roll (fn [n] (inc (mod (dec n) 9)))
        nxt (apply concat
              (for [rbos (range 5)]
                (for [r risk]
                  (apply concat
                    (for [cbos (take 5 (drop rbos (range)))]
                      (map #(roll (+ cbos %)) r))))))
        rows (count nxt)
        cols (count (first nxt))]
    {:map nxt :rows rows :rmax (dec rows) :cols cols :cmax (dec cols) :start [0 0] :end [(dec rows) (dec cols)]}))

(defn two
  "Function to find the lowest cost of traveling through the jumbo
  version of the cave based on the jumbo risk map, and return the total
  value of the risk we encountered."
  [& [coll]]
  (map-it (jumbo puzzle)))

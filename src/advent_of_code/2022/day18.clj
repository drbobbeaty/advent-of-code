(ns advent-of-code.2022.day18
  "Eighteenth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the lava droplets as they fall into the water to cool."
  (-> (slurp "resources/2022/input/day18.txt")
    (trim)
    (split #"\n")
    (->> (map #(split % ","))
     (map parse-int))))

(def test1
  "Test data for the first part."
  (-> ["2,2,2"
       "1,2,2"
       "3,2,2"
       "2,1,2"
       "2,3,2"
       "2,2,1"
       "2,2,3"
       "2,2,4"
       "2,2,6"
       "1,2,5"
       "3,2,5"
       "2,1,5"
       "2,3,5"]
    (->> (map #(split % ","))
     (map parse-int))))

(defn build
  "Function to buuild an index of the droplets flattened to 2D for each of
  the axes, and then a set for all the off-plane values. This will make it
  easy to search the data."
  [coll]
  (let [data (atom {})]
    (doseq [[x y z] coll
            :let [zn (conj (get @data [x y 0] #{}) z)
                  yn (conj (get @data [x 0 z] #{}) y)
                  xn (conj (get @data [0 y z] #{}) x)]]
      (swap! data assoc [x y 0] zn [x 0 z] yn [0 y z] xn))
    @data))

(defn one
  "Function to find the total number of exposed sides of all droplets."
  [& [coll]]
  (let [game (or coll puzzle)
        idx (build game)
        tst (fn [a] (if (nil? a) 0 1))]
    (loop [src game
           cnt (* 6 (count game))]
      (if-let [[x y z] (first src)]
        (let [xy (get idx [x y 0])
              xz (get idx [x 0 z])
              yz (get idx [0 y z])
              hits (+ (tst (xy (inc z))) (tst (xy (dec z)))
                      (tst (xz (inc y))) (tst (xz (dec y)))
                      (tst (yz (inc x))) (tst (yz (dec x))))]
          (recur (rest src) (- cnt hits)))
        {:count cnt}))))

(defn foo
  ""
  []
  (build test1))

(defn two
  "Function to find the "
  [& [coll]]
  ;; needs a flood-fill and I'm not interested in doing it now.
  )

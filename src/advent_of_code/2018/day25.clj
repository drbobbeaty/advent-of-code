(ns advent-of-code.2018.day25
  "Twenty-fifth day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.2016.day25 :refer [->int]]
            [advent-of-code.2018.day23 :refer [dist]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "These are the stars in space/time that will open up the portal for the
  milk chocolate for the reindeer to get better."
  (->> "resources/2018/input/day25.txt"
    (io/reader)
    (line-seq)
    (map #(mapv ->int (cs/split % #",")))))

(def sample-1
  "Sample data from the puzzle - should be 2 constellations"
  (->> ["0,0,0,0"
        "3,0,0,0"
        "0,3,0,0"
        "0,0,3,0"
        "0,0,0,3"
        "0,0,0,6"
        "9,0,0,0"
       "12,0,0,0"]
    (map #(mapv ->int (cs/split % #",")))))

(def sample-2
  "Sample data from the puzzle - should be 4 constellations"
  (->> ["-1,2,2,0"
        "0,0,2,-2"
        "0,0,0,-2"
        "-1,2,0,0"
        "-2,-2,-2,2"
        "3,0,2,-1"
        "-1,3,2,2"
        "-1,0,-1,0"
        "0,2,1,-2"
        "3,0,0,0"]
    (map #(mapv ->int (cs/split % #",")))))

(def sample-3
  "Sample data from the puzzle - should be 3 constellations"
  (->> ["1,-1,0,1"
        "2,0,-1,0"
        "3,2,-1,0"
        "0,0,3,1"
        "0,0,-1,-1"
        "2,3,-2,0"
        "-2,2,0,0"
        "2,-2,0,-1"
        "1,-1,0,-1"
        "3,2,0,2"]
    (map #(mapv ->int (cs/split % #",")))))

(def sample-4
  "Sample data from the puzzle - should be 8 constellations"
  (->> ["1,-1,-1,-2"
        "-2,-2,0,1"
        "0,2,1,3"
        "-2,3,-2,1"
        "0,2,3,-2"
        "-1,-1,1,-2"
        "0,-2,-1,0"
        "-2,2,3,-1"
        "1,2,2,0"
        "-1,-2,0,-2"]
    (map #(mapv ->int (cs/split % #",")))))

(defn cluster
  "Function to cluster the starts into constellations so that we can see how
  many constellations there are in the data for the portal for the milk
  chocolate."
  [s]
  (let [grps (atom [])]
    (doseq [p s
            :let [near? (fn [x] (<= (dist x p) 3))
                  hits (filter #(some near? %) @grps)]]
      (if (zero? (count hits))
        (swap! grps conj [p])
        (reset! grps (conj (remove (set hits) @grps) (conj (apply concat hits) p)))))
    {:count (count @grps) :groups @grps}))

(defn one
  "Function to get the count of the constellations in the puzzle input."
  []
  (:count (cluster puzzle)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (cluster sample-4))

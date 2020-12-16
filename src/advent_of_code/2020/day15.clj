(ns advent-of-code.2020.day15
  "Fifteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of "
  [5 1 9 18 13 8 0])

(def test1
  "Test data for the first part."
  [0 3 6])

(defn nxt
  "Function to return the next number in the provided sequence, based on the
  rules of the elves' game."
  [coll]
  (let [[f & mre] (reverse coll)
        idx (.indexOf mre f)]
    (if (= -1 idx) 0 (inc idx))))

(defn one
  "Function to play the game, getting the next number in the sequence based
  on the elves' game, and tacking it onto the end of the sequence, and keep
  going until we get to the 2020th number spoken."
  [& [coll]]
  (loop [src (vec (or coll puzzle))]
    (if (<= 2020 (count src))
      (last src)
      (recur (conj src (nxt src))))))

(defn two
  "Function to do basically the same thing, but much longer, and this time
  we're not searching the string, we're using a map for the last time we
  spoke a number, and this is fast enough to do the job at hand."
  [& [coll]]
  (let [inp (or coll puzzle)
        limit 30000000
        said (atom 0)]
    (loop [turns (range (inc (count inp)) (inc limit))
           ltsm (transient (zipmap inp (range 1 (inc (count inp)))))
           lts nil]
      (if-let [t (first turns)]
        (let [sn (if (nil? lts) 0 (- (dec t) lts))
              nlts (get ltsm sn)]
          (if (= t limit) (reset! said sn))
          (recur (rest turns) (assoc! ltsm sn t) nlts))))
    @said))

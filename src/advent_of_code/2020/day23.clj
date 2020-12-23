(ns advent-of-code.2020.day23
  "Twenty-third day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input "
  [8 5 3 1 9 2 6 4 7])

(def test1
  "Test data for the first part."
  [3 8 9 1 2 5 4 6 7])

(defn turn
  "Function to take one turn of the Crab Cups game and return the cups with
  the first in the sequence being the 'next cup up', as opposed to rotating
  a pointer to which that one is. This is just easier."
  [cups]
  (let [cc (first cups)
        pc (take 3 (rest cups))
        rc (concat [cc] (drop 4 cups))
        nc (loop [tgt (dec cc)]
             (if (<= 0 (.indexOf rc tgt))
               tgt
               (let [lc (apply min rc)
                     hc (apply max rc)
                     ntgt (dec tgt)]
                 (recur (if (<= lc ntgt hc) ntgt hc)))))
        [fr ba] (split-at (inc (.indexOf rc nc)) rc)
        [a b] (split-at 1 (concat fr pc ba))]
    (vec (concat b a))))

(defn one
  "Function to play the game of 100 moves and return the order of the cups
  after the cup labeled '1', going clockwise around the circle."
  [& [coll]]
  (loop [cups (or coll puzzle)
         cnt 0]
    (if (< cnt 100)
      (recur (turn cups) (inc cnt))
      (let [[a b] (split-at (.indexOf cups 1) cups)]
        (cs/join "" (rest (vec (concat b a))))))))

(defn two
  "Function to play the "
  [& [coll]]
  (let [inp (or coll test1)
        scs (concat inp (range (inc (apply max inp)) 1000001))
       ]
    (loop [cups scs
           cnt 0]
      (if (< cnt 100)
        (recur (turn cups) (inc cnt))
        (let [[a b] (split-at (.indexOf cups 1) cups)]
          (take 3 (vec (concat b a)))
          )))
  ))

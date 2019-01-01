(ns advent-of-code.2017.day11
  "Eleventh day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.hex-grid :as hg]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the sequence of steps the child process took to get to me."
  (-> (slurp "resources/2017/input/day11.txt")
    (cs/trim)
    (cs/split #",")))

(def samples
  "These are all the examples on the puzzle that we need to test to make
  sure that we have all this properly accounted for."
  [["ne" "ne" "ne"]
   ["ne" "ne" "sw" "sw"]
   ["ne" "ne" "s" "s"]
   ["se" "sw" "se" "sw" "sw"]])

(defn one
  "Function to compute how far away we are after taking a walk in the form
  of the puzzle input, where back-tracking is possible, and all that."
  []
  (hg/dist [0 0] (hg/move [0 0] puzzle)))

(defn two
  "Function to not only get the distance at the end of the walk, but to
  get the maximum disance away we were while on the walk. This is an
  interesting twist, and so we did one step at a time."
  []
  (loop [pos [0 0]
         stps puzzle
         maxd 0]
    (if-let [stp (first stps)]
      (let [np (hg/move pos stp)]
        (recur np (rest stps) (max maxd (hg/dist np [0 0]))))
      {:dist (hg/dist pos [0 0]) :max-dist maxd})))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (for [s samples]
    {:path s :dist (hg/dist [0 0] (hg/move [0 0] s))}))

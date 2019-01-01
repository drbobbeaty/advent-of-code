(ns advent-of-code.2017.day17
  "Seventeenth day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the skip-factor for my spinlock."
  356)

(def sample
  "This is the skip-factor for the sample spinlock."
  3)

(defn one
  "Function to build the spinlock, and return it once it is built. But when
  we have added the 2017th value, log what the *next* value is as it's right
  there, at the beginning of the second part of the split."
  []
  (loop [cb [0]
         pos 0
         nval 1]
    (if (<= nval 2017)
      (let [np (inc (mod (+ pos puzzle) (count cb)))
            [fp sp] (split-at np cb)]
        (if (= nval 2017) (infof "after 2017: %s" (first sp)))
        (recur (concat fp [nval] sp) np (inc nval)))
      cb)))

(defn two
  "Function to find the last insert after 50 mil inserts, and the key to this
  is not in doing it, but in seeing the pattern in the inserted values. This
  took some looking at the other solutions to get, but wow... not trivial."
  []
  (let [ladd (atom 0)]
    (loop [i 0
           k 1]
      (if (<= k 50000001)
        (let [i' (inc (mod (+ i puzzle) k))]
          (if (== 1 i') (reset! ladd k))
          (recur i' (inc k)))
        @ladd))))

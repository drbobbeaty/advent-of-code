(ns advent-of-code.2017.day05
  "Fifth day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the jump sequence we need to run through for the puzzle."
  (-> (slurp "resources/2017/input/day05.txt")
    (cs/split #"\n")
    (as-> s (mapv parse-int s))))

(def sample
  "This is the example data in the puzzle."
  [0 3 0 1 -3])

(defn one
  "Function to use the 'always increment' rules on the PC to count how many
  steps it will take to get the program to exit it's normal instruction set."
  []
  (loop [jmps puzzle
         pc 0
         cnt 0]
    (if-let [inst (nth jmps pc nil)]
      (recur (assoc jmps pc (inc inst)) (+ inst pc) (inc cnt))
      {:count cnt})))

(defn two
  "Function to use the more complex rules on the PC to count how many
  steps it will take to get the program to exit it's normal instruction set."
  []
  (let [njv (fn [v] (if (<= 3 v) (dec v) (inc v)))]
    (loop [jmps puzzle
           pc 0
           cnt 0]
      (if-let [inst (nth jmps pc nil)]
        (recur (assoc jmps pc (njv inst)) (+ inst pc) (inc cnt))
        {:count cnt}))))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (let [njv (fn [v] (if (<= 3 v) (dec v) (inc v)))]
    (loop [jmps sample
           pc 0
           cnt 0]
      (if-let [inst (nth jmps pc nil)]
        (recur (assoc jmps pc (njv inst)) (+ inst pc) (inc cnt))
        {:count cnt}))))

(ns advent-of-code.2020.day09
  "Ninth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the instructions in the game console."
  (-> (slurp "resources/2020/input/day09.txt")
      (trim)
      (split #"\n")
      (->> (map parse-int))))

(def test1
  "Test data for the first part - with a preamble of 5."
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn makeit?
  "Predicate function to look at a collection of numbers and a target value,
  and determines if any two numbers in the collection can sum to the target.
  If so, then this returns true, if not, false."
  [coll x]
  (boolean (some #(= x (apply + %)) (combinations coll 2))))

(defn one
  "Function to look at the collection of XMAS data and using a 25 number
  preamble, finds the first number that can't be created from the sum of
  two numbers in the previous 25 (the size of the preamble)."
  [& [coll]]
  (let [src (or coll puzzle)
        sz (if (< 50 (count src)) 25 5)]
    (first
      (for [win (partition (inc sz) 1 src)
            :let [pre (drop-last win)
                  tgt (last win)]
            :when (not (makeit? pre tgt))]
        tgt))))

(defn lead-sum
  "Function to look at the leading numbers in the provided collection, and
  find out how many will sum to the target value, x. If the first 'n' do not
  sum to x - either we run out, or they overshoot x, then return nil."
  [coll x]
  (loop [src coll
         tot 0
         pts (transient [])]
    (if-let [f (first src)]
      (let [ntot (+ f tot)]
        (cond
          (< ntot x)
            (recur (rest src) ntot (conj! pts f))
          (= x ntot)
            (persistent! (conj! pts f)))))))

(defn two
  "Function to look in the provided collection a sequence of contiguous
  numbers that sum to the result of part one for the same sequence. Once
  we have this sequence, take the extremes and sum them for the XMAS
  weakness value that is the answer to part 2."
  [& [coll]]
  (let [src (or coll puzzle)
        tgt (one src)
        grp (loop [lst src]
              (if-let [ls (lead-sum lst tgt)]
                ls
                (recur (rest lst))))
        lo (apply min grp)
        hi (apply max grp)]
    {:seq grp :min lo :max hi :weakness (+ lo hi)}))

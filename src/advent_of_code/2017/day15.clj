(ns advent-of-code.2017.day15
  "Fourteenth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(def puzzle
  "These are the starting numbers for the generators in today's puzzle."
  {:a 618 :b 814})

(def sample
  "These are the starting numbers for the generators in the sample data."
  {:a 65 :b 8921})

(defn one
  "Function to run through 40 mil pairs of A and B, and see how many times the
  lower 16-bits of each match up. This was far faster than using sequences."
  []
  (let [a (atom (:a puzzle))
        b (atom (:b puzzle))
        hits (atom 0)]
    (doseq [i (range 40000000)
            :let [a' (mod (* 16807 @a) 2147483647)
                  b' (mod (* 48271 @b) 2147483647)]]
      (reset! a a')
      (reset! b b')
      (if (= (bit-and a' 0xffff) (bit-and b' 0xffff))
        (swap! hits inc)))
    @hits))

(defn two
  "Function to run through 5 mil pairs of A and B, and see how many times the
  lower 16-bits of each match up. This had to skip values in A and B based on
  yet another criteria, and really wouldn't have been nicely suited for a
  sequence."
  []
  (let [a (atom (:a puzzle))
        b (atom (:b puzzle))
        hits (atom 0)]
    (doseq [i (range 5000000)
            :let [a' (loop [x @a]
                       (let [x' (mod (* 16807 x) 2147483647)]
                         (if-not (zero? (mod x' 4))
                          (recur x')
                          x')))
                  b' (loop [x @b]
                       (let [x' (mod (* 48271 x) 2147483647)]
                         (if-not (zero? (mod x' 8))
                          (recur x')
                          x')))]]
      (reset! a a')
      (reset! b b')
      (if (= (bit-and a' 0xffff) (bit-and b' 0xffff))
        (swap! hits inc)))
    @hits))

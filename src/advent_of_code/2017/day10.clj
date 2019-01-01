(ns advent-of-code.2017.day10
  "Tenth day's solutions for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int sum]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the sequence of items in the stream as it flows by."
  (-> (slurp "resources/2017/input/day10.txt")
    (cs/trim)
    (cs/split #",")
    (as-> s (map parse-int s))))

(def ascii-puzzle
  "This is the puzzle input for the second half of the day, where we need
  to treat it as ascii, and convert it to integers, and add a few to the
  end for good measure."
  (-> (slurp "resources/2017/input/day10.txt")
    (cs/trim)
    (as-> s (map int s))
    (concat [17 31 73 47 23])))

(defn rev
  "Function to take a vector, and reverse the 'len' of values starting at 'pos'
  and return the result. This will wrap-around the end of the sequence, in
  the event that the length and position are such that they need more characters
  in the reversal."
  [sn pos len]
  (let [nums (atom sn)
        sz (count sn)]
    (doseq [os (range (quot len 2))
            :let [idx (mod (+ pos os) sz)
                  jdx (mod (- (+ pos (dec len)) os) sz)
                  t (nth @nums jdx)]]
      (swap! nums assoc jdx (nth @nums idx))
      (swap! nums assoc idx t))
    @nums))

(defn a-round
  "Function to compute one round of the knot hash for the puzzle input on
  the input sequence of numbers (sn), with a starting position (sp), and
  a starting 'skip' value (ss). The list of lengths to process (ls) are
  also provided, and are the key on what to do."
  [sn sp ss ls]
  (loop [nums (vec (or sn (range 256)))
         pos (or sp 0)
         skip (or ss 0)
         lens (or ls puzzle)]
    (if-let [l (first lens)]
      (recur (rev nums pos l) (mod (+ pos l skip) (count nums)) (inc skip) (rest lens))
      {:numbers nums :pos pos :skip skip})))

(defn knot
  "Function to compute the knot hash based on the provided sequence of
  numbers and the list of lengths to use, and it returns the hex string
  of the hash."
  [sn ls]
  (->> (loop [nums (vec (or sn (range 256)))
              rnds (range 64)
              pos 0
              skip 0]
         (if-let [r (first rnds)]
           (let [{n :numbers p :pos s :skip} (a-round nums pos skip (or ls ascii-puzzle))]
             (recur n (rest rnds) p s))
           nums))
    (partition 16)
    (map #(format "%02x" (apply bit-xor %)))
    (apply str)))

(defn one
  "Function to compute the knot hash for the puzzle input on a 256-element
  string. And then multiply the first two elements of the result together,
  and return that."
  []
  (->> (a-round (range 256) 0 0 puzzle)
    (:numbers)
    (take 2)
    (apply *)))

(defn two
  "Function to compute the complete knot hash of the puzzle input on a
  256-length string."
  []
  (knot (range 256) ascii-puzzle))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (->> (loop [nums (vec (range 256))
              rnds (range 64)
              pos 0
              skip 0]
         (if-let [r (first rnds)]
           (let [{n :numbers p :pos s :skip} (a-round nums pos skip ascii-puzzle)]
             (recur n (rest rnds) p s))
           nums))
    (partition 16)
    (map #(format "%02x" (apply bit-xor %)))
    (apply str)
  ))

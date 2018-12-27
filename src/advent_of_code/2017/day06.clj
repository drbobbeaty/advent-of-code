(ns advent-of-code.2017.day06
  "Sixth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(def puzzle
  "This is the jump sequence we need to run through for the puzzle."
  (-> (slurp "resources/2017/input/day06.txt")
    (cs/split #"\s")
    (as-> s (mapv parse-int s))))

(def sample
  "This is the example data in the puzzle."
  [0 2 7 0])

(defn distribute
  "Function to 'distribute' the blocks (third arg) among the sequence of blocks
  (the first arg) starting with the index (the second arg). This reads,
  distribute into these bocks, starting at this index, this number of blocks.
  The resulting value is the updated sequence of blocks."
  [s si nd]
  (loop [bnks s
         indx si
         drp nd]
    (if (pos? drp)
      (recur (update bnks indx inc) (mod (inc indx) (count bnks)) (dec drp))
      bnks)))

(defn one
  "Function to keep redistributing blocks, based on the rules in the puzzle,
  until there is a repeated pattern in the blocks, and then stop. This is to
  keep it from being an infinite loop."
  []
  (loop [bnks puzzle
         seen #{}
         cnt  0]
    (let [mbv (apply max bnks)
          idx (.indexOf bnks mbv)
          nxt (distribute (assoc bnks idx 0) (mod (inc idx) (count bnks)) mbv)]
      (if-not (seen nxt)
        (recur nxt (conj seen nxt) (inc cnt))
        {:banks nxt :count (inc cnt)}))))

(defn two
  "Function to not only detect the repeated banks configuration, but to
  also return the number of cycles between the repeated values. This may
  not at all be the number of cycles to find that value. This is more
  expensive because we have to use a non-indexed search, but it gets us
  the values we need."
  []
  (loop [bnks puzzle
         seen []
         cnt  0]
    (let [mbv (apply max bnks)
          idx (.indexOf bnks mbv)
          nxt (distribute (assoc bnks idx 0) (mod (inc idx) (count bnks)) mbv)]
      (if-not (pos? (.indexOf seen nxt))
        (recur nxt (conj seen nxt) (inc cnt))
        {:banks nxt :count (inc cnt) :cycles (- (count seen) (.indexOf seen nxt))}))))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (loop [bnks sample
         seen []
         cnt  0]
    (let [mbv (apply max bnks)
          idx (.indexOf bnks mbv)
          nxt (distribute (assoc bnks idx 0) (mod (inc idx) (count bnks)) mbv)]
      (infof "banks: %s ... max: %s ... idx: %s ... next: %s" bnks mbv idx nxt)
      (if-not (pos? (.indexOf seen nxt))
        (recur nxt (conj seen nxt) (inc cnt))
        {:banks nxt :count (inc cnt) :cycles (- (count seen) (.indexOf seen nxt))}))))

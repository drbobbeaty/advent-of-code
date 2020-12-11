(ns advent-of-code.2020.day10
  "Tenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split partition-on-diff]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the instructions in the game console."
  (-> (slurp "resources/2020/input/day10.txt")
      (trim)
      (split #"\n")
      (->> (map parse-int))))

(def test1
  "Test data for the first part, the first set of adapters."
  [16 10 15 5 1 11 7 19 6 12 4])

(def test2
  ""
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn one
  "Function to use all the available adapters in a chain where they can only
  be less than 4 jolts difference from one to another, and always increasing.
  Then look at the differences between steps, and count the 1 jolt jumps and
  the 3 jolt jumps, and multiply those numbers together."
  [& [coll]]
  (let [src (or coll puzzle)
        me (+ 3 (apply max src))
        difs (for [[a b] (partition 2 1 (concat [0] (sort src) [me]))] (- b a))
        one (count (filter #(= 1 %) difs))
        tre (count (filter #(= 3 %) difs))]
    {:1 one :3 tre :prod (* one tre)}))

(defn cnt-drops
  "Function to count the potential drops in the sequence based on the joltage
  difference of 3. This is an ugly brute-force method that loops over all
  the possible drops in the sequence, and does it for single- and double-
  drops. The collection is the remaining sequence to scan, and the 'sz' is
  3 for single-number drops, and 4 for double-number drops. These are the
  sizes of the first 'n' elements to test for the potential drop(s)."
  [coll sz]
  (loop [src coll
         drops 0]
    (if (<= sz (count src))
      (let [ss (take sz src)]
        (if (<= (- (last ss) (first ss)) 3)
          (let [rsrc (drop (dec sz) src)]
            (recur (rest src) (+ (inc drops) (cnt-drops rsrc 3) (cnt-drops rsrc 4))))
          (recur (rest src) drops)))
      drops)))

(defn two
  "Function to find out how many sequences of adapters can be made from
  the provided list. This is a big problem if we don't break it up, so
  we realize that a delta of 3 breaks the sequence into pieces, and from
  that, we can compute the individual counts, and multiply them together."
  [& [coll]]
  (let [inp (or coll puzzle)]
    (->> (concat [0] (sort inp) [(+ 3 (apply max inp))])
         (partition-on-diff 3)
         (map #(+ 1 (cnt-drops % 3) (cnt-drops % 4)))
         (apply *))))

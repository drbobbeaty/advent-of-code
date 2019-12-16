(ns advent-of-code.2019.day16
  "Sixteenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.logging :refer [log-execution-time!]]
            [advent-of-code.util :refer [parse-int sum ascii-0]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of signal from the elves that needs to be decoded."
  (-> (slurp "resources/2019/input/day16.txt")
      (cs/trim)
      (->> (map #(- (int %) ascii-0)))))

(defn phase
  "Function to compute one phase of the FFT for the input data stream."
  [s]
  (for [i (range 1 (inc (count s)))
        :let [ubp (concat (repeat i 0) (repeat i 1) (repeat i 0) (repeat i -1))
              bp (rest (apply concat (repeat ubp)))]]
    (mod (abs (apply + (map (fn [x i] (case i 0 0 1 x (-' x))) s bp))) 10)))

(log-execution-time! phase)

(defn yoyo
  "Test some of the examples in the puzzle description - to make sure."
  []
  (for [t ["80871224585914546619083218645595" "19617804207202209144916044189917" "69317163492948606335995924319873"]
        :let [inp (map #(- (int %) ascii-0) t)]]
    (take 8 (first (drop 100 (iterate phase inp))))))

(defn one
  "Function to run the puzzle input as the signal throught the FFT 100 times,
  and then grap the first 8 digits of the result."
  [& [coll]]
  (take 8 (first (drop 100 (iterate phase (or coll puzzle))))))

(log-execution-time! one)

(defn mrbig
  "Function to take a sequence of digits, and then create a sequence of their
  sums - each element droping one element from the front - making a upper
  diagonal of numbers. This is what's happening on the latter-half of the
  phase calculation, and since part 2 is all about the latter half, this is
  a much faster way to do it."
  [s]
  (loop [src (reverse s)
         ans (transient [])
         tot 0]
    (if-let [x (first src)]
      (recur (rest src) (conj! ans (mod (abs (+ tot x)) 10)) (+ tot x))
      (reverse (persistent! ans)))))

(defn two
  "Function to take the offset from the input data, and then realize that this
  will chop off more than half of the stream. Because of this, the work that
  the phase change does in this part of the data is summing the remaining
  elements in the data. So we can make a 'mrbig' to do that easily and very
  efficiently, and then rip that 100 times, and we're good to go."
  [& [coll]]
  (let [sig (apply concat (repeat 10000 (or coll puzzle)))
        os (parse-int (apply str (take 7 sig)))]
    (loop [seg (drop os sig)
           cnt 0]
      (infof "on step %d/%d : %s" cnt 100 (cs/join "" (take 8 seg)))
      (if (< cnt 100)
        (recur (mrbig seg) (inc cnt))
        (take 8 seg)))))

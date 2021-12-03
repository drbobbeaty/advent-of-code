(ns advent-of-code.2021.day03
  "Third day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the diagnostic report from the sub's system."
  (-> (slurp "resources/2021/input/day03.txt")
    (trim)
    (split #"\n")
    (->> (map #(map parse-int (seq %))))))

(def test1
  "Test data for the first part."
  (-> ["00100"
       "11110"
       "10110"
       "10111"
       "10101"
       "01111"
       "00111"
       "11100"
       "10000"
       "11001"
       "00010"
       "01010"]
    (->> (map #(map parse-int (seq %))))))

(defn transpose
  "Function to take a sequence of sequences, each of the same length and
  transpose the marix of values, turning rows into columns and returning
  the result."
  [coll]
  (for [c (range (count (first coll)))]
    (map #(nth % c) coll)))

(defn to-dec
  "Function to take a sequence of binary digits and convert them into a
  single decimal number. This is just calculating the powers of 2, and then
  multiplying each digit by the corresponding factor and summing the
  products."
  [coll]
  (->> (iterate #(* 2 %) 1)
    (take (count coll))
    (reverse)
    (map vector coll)
    (map #(* (first %) (last %)))
    (sum)))

(defn one
  "Function to find the power consumption of the sub by first calculating
  the gamma and epsilon from the diagnostic report data, and then making the
  product of the two."
  [& [coll]]
  (let [hits (for [r (transpose puzzle)] (frequencies r))
        gamma (to-dec (for [d hits] (if (< (get d 0) (get d 1)) 1 0)))
        epsilon (to-dec (for [d hits] (if (< (get d 0) (get d 1)) 0 1)))]
    {:gamma gamma :epsilon epsilon :power (* gamma epsilon)}))

(defn p-filter
  "Function to take a collection of binary sequences and a test function
  for what to match on for each collection of digit positions, and will
  then follow the filtering rules as we move along the binary digits to
  filter out those that don't match the output of the function f, whose
  input is the frequency count of the digits in thta position."
  [coll f]
  (loop [i 0
         hits coll]
    (if (< 1 (count hits))
      (let [d (frequencies (map #(nth % i) hits))]
        (recur (inc i) (filter #(= (nth % i) (f d)) hits)))
      (first hits))))

(defn two
  "Function to find the oxygen and co2 numbers, along with their product,
  the life support number for the sub - based on a different scheme of
  manipulating the diagnostic report."
  [& [coll]]
  (let [oxy (to-dec (p-filter puzzle #(if (<= (get % 0) (get % 1)) 1 0)))
        co2 (to-dec (p-filter puzzle #(if (<= (get % 0) (get % 1)) 0 1)))]
    {:oxy oxy :co2 co2 :life (* oxy co2)}))

(ns advent-of-code.2018.day01
  "First day's solutions for the Advent of Code 2018"
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the frequency changes for the puzzle."
  (-> (slurp "resources/2018/input/day01.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map #(Integer/parseInt (cs/trim %))))))

(defn one
  "Function to take an input of the frequency changes, and then compute
  the resulting frequency."
  [& [src start]]
  (+ (or start 0) (reduce + (or src puzzle))))

(defn two
  "Function to use the input of frequency changes (repeating), to find the
  first _repeated_ frequency in the change sequence regardless of how long
  it takes to find that duplicate."
  [& [src start]]
  (loop [chgs (flatten (repeat (or src puzzle)))
         hits #{}
         freq (or start 0)]
    (let [nf (+ (first chgs) freq)]
      (if (hits nf)
        nf
        (recur (rest chgs) (conj hits nf) nf)))))

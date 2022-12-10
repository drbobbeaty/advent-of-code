(ns advent-of-code.2022.day10
  "Tenth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of instructions for the CPU to drive the CRT in my
  communications device."
  (-> (slurp "resources/2022/input/day10.txt")
    (trim)
    (split #"\n")))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2022/input/day10test.txt")
    (trim)
    (split #"\n")))

(defn signal
  "Function to take a program of instructions for the CRT and create a time
  series signal that can be interpreted by the caller. These are the X
  values at the *middle* of the cycle they appear in the sequence."
  [coll]
  (loop [x 1
         cyc 1
         sig [1]
         pgm coll]
    (if-let [l (first pgm)]
      (if (= "noop" l)
        (recur x (inc cyc) (conj sig x) (rest pgm))
        (let [nx (+ (parse-int (subs l 5)) x)
              nsig (conj sig x)
              ncyc (inc cyc)]
          (recur nx (inc ncyc) (conj nsig nx) (rest pgm))))
      sig)))

(defn one
  "Function to find the signal strength of the output of the program
  for the CPU."
  [& [coll]]
  (let [sig (signal puzzle)]
    (sum
      (for [i [20 60 100 140 180 220]]
        (* i (nth sig (dec i)))))))

(defn two
  "Function to find the message drawn on the CRT by the CPU."
  [& [coll]]
  (let [sig (signal puzzle)]
    (->> (for [cyc (range 240)
               :let [spm (nth sig cyc)
                     cycx (mod cyc 40)]]
           (if (<= (dec spm) cycx (inc spm)) \# \.))
      (partition 40)
      (map #(apply str %)))))

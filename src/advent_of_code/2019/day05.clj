(ns advent-of-code.2019.day05
  "Fifth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [advent-of-code.2019.day02 :refer [vat]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the "
  (-> (slurp "resources/2019/input/day05.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-int))))

(def trial1
  "Test data for the first part of the puzzle"
  [1002 4 3 4 33])

(def trial2
  "Test data for the first part of the puzzle"
  [1101 100 -1 4 0])

(defn run
  "Function to take the memory map, and a program counter, and execute the
  Intcode program starting at that address and ending when the program
  successfully exits."
  [mems & [arg pc]]
  (loop [mem mems
         ip (or pc 0)]
    (let [inst (nth mem ip nil)
          op (mod inst 100)
          mde (reverse (drop-last 2 (str inst)))
          ld (fn [ipos]
               (if (= \1 (nth mde (dec ipos) \0))
                 (nth mem (+ ipos ip) nil)
                 (vat mem (+ ipos ip))))
          not-zero? (comp not zero?)]
      (case op
        (1 2) (let [a (ld 1)
                    b (ld 2)
                    tgt (nth mem (+ 3 ip) nil)]
                (recur (assoc mem tgt ((if (= op 1) + *) a b)) (+ 4 ip)))
        3     (let [inp (or arg 0)
                    tgt (nth mem (+ 1 ip) nil)]
                (recur (assoc mem tgt inp) (+ 2 ip)))
        4     (let [out (ld 1)]
                (prn out)
                (recur mem (+ 2 ip)))
        (5 6) (let [tst (ld 1)
                    nip (ld 2)]
                (recur mem (if ((if (= op 5) not-zero? zero?) tst) nip (+ 3 ip))))
        (7 8) (let [a (ld 1)
                    b (ld 2)
                    tgt (nth mem (+ 3 ip) nil)]
                (recur (assoc mem tgt (if ((if (= op 7) < =) a b) 1 0)) (+ 4 ip)))
        99    mem
        mem))))

(defn yoyo
  "Run a bunch of tests on the trial input. The output should be a series of
  0, 1, ... 999, 1000, 1001 - which means it's all working as it's supposed
  to."
  []
  (doseq [[mem inp] [[[3 9 8 9 10 9 4 9 99 -1 8] 5] [[3 9 8 9 10 9 4 9 99 -1 8] 8]
                     [[3 9 7 9 10 9 4 9 99 -1 8] 9] [[3 9 7 9 10 9 4 9 99 -1 8] 7]
                     [[3 3 1108 -1 8 3 4 3 99] 5] [[3 3 1108 -1 8 3 4 3 99] 8]
                     [[3 3 1107 -1 8 3 4 3 99] 9] [[3 3 1107 -1 8 3 4 3 99] 7]
                     [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 0]
                     [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 1]
                     [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0]
                     [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 1]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 7]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 8]
                     [[3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] 9]
                     ]]
    (run mem inp)))

(defn bobo
  "Testing out the one simluation looking for an issue. Got it!"
  []
  (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0))

(defn one
  "Function to run the program with an input of 1, and toss the memory at
  the end because the printing output will be the log of all we need."
  [& coll]
  (let [inp (vec (or coll puzzle))]
    (run inp 1)
    :done))

(defn two
  "Function to run the program with an input of 5, and toss the memory at
  the end because the printing output will be the log of all we need."
  [& coll]
  (let [inp (vec (or coll puzzle))]
    (run inp 5)
    :done))

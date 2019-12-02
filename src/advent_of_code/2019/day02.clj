(ns advent-of-code.2019.day02
  "Second day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the frequency changes for the puzzle."
  (-> (slurp "resources/2019/input/day02.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map #(parse-int %)))))

(defn vat
  "Function to get the value at the memory location pointed to by 'k'. This
  means we need to first get the value at 'k', and then get the value at that
  location in the array. If something isn't right - return nil."
  [s k]
  (if-let [idx (nth s k nil)]
    (nth s idx nil)))

(defn step
  "Function to take the memory map, and a program counter, and execute the
  Intcode program starting at that address and ending when the program
  successfully exits."
  [mem & [pc]]
  (if-let [op (nth mem (or pc 0) nil)]
    (case op
      (1 2) (let [ip (or pc 0)
                  a (vat mem (+ 1 ip))
                  b (vat mem (+ 2 ip))
                  tgt (nth mem (+ 3 ip) nil)]
              (step (assoc mem tgt ((if (= op 1) + *) a b)) (+ 4 ip)))
      99    mem)
    mem))

(defn one
  "Function to take the puzzle input and replace the second and third
  values in memory with 12 and 2 - respectively, as instructed in the
  problem statement. This then will run to completion, and we'll be able
  to answer the question to save Santa."
  [& coll]
  (let [inp (vec (or coll puzzle))
        fixed (-> inp
                (assoc 1 12)
                (assoc 2 2))]
    (step fixed)))

(defn run
  "Function to take a noun and verb and place them in memory locations 1 and 2
  respectively, and then run the provided puzzle program to it's completion
  and then return the value at location 0. This is just about 'one', but we
  wanted to make it clear to make sure that we had reference values that
  passed the tests in the puzzle."
  [noun verb]
  (let [inp (-> (vec puzzle)
              (assoc 1 noun)
              (assoc 2 verb))]
    (first (step inp))))

(defn two
  "Function to find the noun and verb that will return the value specified in
  the puzzle. This is just a simple scan of all possible values, stopping at
  the first one that matches. Then we compute the value to save Santa, and
  that's it."
  []
  (->> (for [n (range 100)
             v (range 100)
             :when (= (run n v) 19690720)]
         [n v (+ (* 100 n) v)])
       (first)))

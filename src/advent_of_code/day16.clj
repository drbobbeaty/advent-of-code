(ns advent-of-code.day16
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the Aunt Sue breakdowns."
  (into {} (for [l (cs/split-lines (slurp "resources/input/day16.txt"))
                 :let [parts (.split (cs/replace (cs/replace l #":|," "") #"Sue " "Sue_") " ")
                       prop (into {} (for [[k v] (partition 2 (rest parts))]
                                       [(keyword k) (Integer/parseInt v)]))]]
    [(first parts) prop])))

(def mfcsam
  "This is the output of the MFCSAM on the ribbon sample."
  {:children 3, :cats 7, :samoyeds 2, :pomeranians 3, :akitas 0, :vizslas 0,
   :goldfish 5, :trees 3, :cars 2, :perfumes 1})

(defn match?
  "Function to provide an exact match of the first argument into the second,
  based on the keys of the first. Missing keys in the first that are in the
  second are ignored."
  [m1 m2]
  (every? identity (for [[k v] m1]
                     (= v (get m2 k)))))

(defn almost?
  "Function to correctly identify the match based on the incorrect readings
  of the MFCSAM."
  [m1 m2]
  (every? identity (for [[k v] m1]
                     (cond
                        (= :cats k)        (> v (get m2 k))
                        (= :trees k)       (> v (get m2 k))
                        (= :pomeranians k) (< v (get m2 k))
                        (= :goldfish k)    (< v (get m2 k))
                        :else              (= v (get m2 k))))))

(defn part1
  "Function to run the exact match on the puzzle and the response"
  []
  (for [[k v] puzzle
        :when (match? v mfcsam)]
    [k v]))

(defn part2
  "Function to run the approximate match on the puzzle and the response"
  []
  (for [[k v] puzzle
        :when (almost? v mfcsam)]
    [k v]))

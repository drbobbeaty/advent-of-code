(ns advent-of-code.2015.day19
  (:require [clojure.math.combinatorics :refer [subsets]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the replacements."
  (for [l (cs/split-lines (slurp "resources/2015/input/day19.txt"))
        :let [[_ src repl] (re-matches #"(.*?) => (.*?)" l)]
        :when (and src repl)]
    [src repl]))

(def medicine
  "This is the initial medicone molecule from Santa."
  "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr")

(defn do-repl
  "Function to do all the possible replacements based on the argument, the source
  string, and it's replacement value. This will be a sequence of strings - one
  per replacement."
  [arg s r]
  (let [parts (.split arg s -2)
        cnt (dec (count parts))]
    (for [i (range cnt)
          :let [rr (concat (repeat i s) [r] (repeat (max 0 (- cnt i 1)) s) [""])]]
      (cs/join (interleave parts rr)))))

(defn part1
  "Function to find out how many unique replacements there are for the
  medicine and the puzzle parts."
  []
  (distinct (apply concat (for [[k v] puzzle] (do-repl medicine k v)))))

(defn a-ok?
  "Predicate function to see if the sequence even stands a ghost of a chance
  to create the medicine. This is a simple pruning scheme that will hopefully
  speed up the search by removing a lot of the dead-ends."
  [cs]
  (let [[s r] (first cs)
        ccf (loop [rms (rest cs)
                   soac r]
              (let [[fs fr] (first rms)]
                (if (and fs fr)
                  (if (<= 0 (.indexOf soac fs))
                    (recur (rest rms) (str soac fr)))
                  (< (count medicine) (count soac))
                  )))]
    (and (= "e" s) ccf)))

(defn dig
  "Function to dig into the branch of replacements to find if there's a
  match with the medicine, and if there is, return true. This branch-based
  search saves memory over the other scheme we had tried."
  [sm ss]
  (if-let [fs (first ss)]
    (some identity (for [mtn (do-repl sm (first fs) (second fs))]
                     (dig mtn (rest ss))))
    (= medicine sm)))

(defn part2
  "Function to try and find the way of generating the medicine from 'e' based
  on the puzzle sequences. This is just a horrible, exhaustive search, but it's
  the only way I know to do it."
  []
  (for [sss (subsets (range (count puzzle)))
        :let [cs (map #(nth puzzle %) sss)]
        :when (and (a-ok? cs) (dig "e" cs))]
    cs))

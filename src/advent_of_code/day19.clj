(ns advent-of-code.day19
  (:require [clojure.math.combinatorics :refer [subsets]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the replacements."
  (for [l (cs/split-lines (slurp "resources/input/day19.txt"))
        :let [[_ src repl] (re-matches #"(.*?) => (.*?)" l)]
        :when (and src repl)]
    [src repl]))

(def medicine
  "This is the initial medicone molecule from Santa."
  "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr")

(defn do-repl
  ""
  [arg s r]
  (let [parts (.split arg s -2)
        cnt (dec (count parts))]
  (for [i (range cnt)
        :let [rr (concat (repeat i s) [r] (repeat (max 0 (- cnt i 1)) s) [""])]]
    (cs/join (interleave parts rr)))))

(defn part1
  ""
  []
  (distinct (apply concat (for [[k v] puzzle] (do-repl medicine k v)))))

(defn a-ok?
  ""
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
    (and (= "e" s) ccf)
       ))

(defn dig
  ""
  [sm ss]
  (if-let [fs (first ss)]
    (some identity (for [mtn (do-repl sm (first fs) (second fs))]
                     (dig mtn (rest ss))))
    (= medicine sm)
   ))

(defn part2
  ""
  []
  (for [sss (subsets (range (count puzzle)))
        :let [cs (map #(nth puzzle %) sss)]
        :when (and (a-ok? cs) (dig "e" cs))
       ]
    cs
    )
    )

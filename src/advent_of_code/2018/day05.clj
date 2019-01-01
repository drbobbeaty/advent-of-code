(ns advent-of-code.2018.day05
  "Fifth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def puzzle
  "This is the source list of all elf fabric claims."
  (-> "resources/2018/input/day05.txt"
      (io/reader)
      (line-seq)))

(defn react
  "Function to run through the incoming polymer and do all the first-level
  reactions that are in the sequence. Return what would be left after the
  reactions have taken place, and removed from the sequence."
  [s]
  (let [mix? (fn [a b] (let [ia (int a)
                             ib (int b)
                             lc (max ia ib)
                             uc (min ia ib)]
               (and (< 64 uc 91 lc 123) (= 32 (- lc uc)))))]
    (loop [ps s
           rp (transient [])]
      (if (< 1 (count ps))
        (let [[fc sc & r] ps]
          (if (mix? fc sc)
            (recur r rp)
            (recur (rest ps) (conj! rp fc))))
        (apply str (persistent! (conj! rp (first ps))))))))

(defn cook
  "Function to react the incoming polymer sequence over and over as long as
  there are reactions to do. Once it's all reacted, then return what's left
  over."
  [s]
  (loop [src s]
    (let [nxt (react src)]
      (if (< (count nxt) (count src))
        (recur nxt)
        nxt))))

(defn one
  "Function to find the length of the puzzle input after it's all reacted.
  We return the original length, and the fully-reacted length."
  [& [src]]
  (let [ps (or src (first puzzle))]
    [(count ps) (count (cook ps))]))

(defn two
  "Function to calculate the *shortest* polymer we can `cook` by removing one
  of the unit types from the source sequence. This will look at the input for
  all the units it contains, and then one-by-one, they will be removed from
  the source, and then cooked to find it's shortest possible polymer."
  [& [src]]
  (let [ps (or src (first puzzle))
        cut (fn [uc lc] (apply str (filter #(and (not= uc %) (not= lc %)) ps)))]
    (apply min (for [uc (set (cs/upper-case ps))
                     :let [lc (char (+ 32 (int uc)))]]
                 (count (cook (cut uc lc)))))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src ["aA" "abBA" "abAB" "aabAAB" "dabAcCaCBAcCcaDA"]]
    (map cook src)))

(defn bobo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [src "dabAcCaCBAcCcaDA"]
    (two src)))

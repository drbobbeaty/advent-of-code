(ns advent-of-code.day11
  (:require [clojure.string :as cs]))

(defn inc-pw
  "Function to find the next potential password by incrementing the LSC by one,
  and if it's 'z', setting it to 'a', and 'carry' the one."
  [s]
  (if (string? s)
    (let [rst (.subSequence s 0 (dec (count s)))
          lc (last s)]
      (cond
        (empty? rst)
          (if (= \z lc) "a" (str (char (inc (int lc)))))
        (= \z lc)
          (str (inc-pw rst) \a)
        :else
          (str rst (char (inc (int lc))))))))

(def runs
  "These are the sequences of characters - one of which has to be in the password."
  (map #(apply str %) (partition 3 1 "abcdefghijklmnopqrstuvwxyz")))

(defn all-runs
  "Function to return the sequences of runs that exist in the provided password."
  [s]
  (filter #(<= 0 (.indexOf s %)) runs))

(def pairs
  "Sequence of all pairs of characters for use in the later predicate function."
  (map #(str % %) "abcdefghijklmnopqrstuvwxyz"))

(defn all-pairs
  "Function to return the sequences of pairs that exist in the provided password."
  [s]
  (filter #(<= 0 (.indexOf s %)) pairs))

(defn iol?
  "Predicate function to indicate the presence of the 'bad characters' i, o, and
  l in the potential password."
  [s]
  (some #{\i \o \l} s))

(defn good?
  "Predicate function to test if the password is valid - based on the rules."
  [s]
  (and (not-empty (all-runs s))
       (not (iol? s))
       (<= 2 (count (all-pairs s)))))

(defn next-pw
  "Find the next valid password for Santa - based on the security rules"
  [s]
  (loop [pw s]
    (let [pw' (inc-pw pw)]
      (if (good? pw')
        pw'
        (recur pw')))))

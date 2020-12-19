(ns advent-of-code.2020.day19
  "Nineteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split un-seq]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a rule string, and parse it into a reasonable data structure
  for processing."
  [s]
  (let [fix (fn [a] (if (<= 0 (.indexOf a "\"")) (second a) (map parse-int (split (trim a) " "))))
        [r mre] (rest (re-matches #"^(\d+): (.+)$" s))]
    [(parse-int r) (un-seq (map fix (split mre "\\|")))]))

(def rules
  "This is the input of the rules for the messages"
  (-> (slurp "resources/2020/input/day19rules.txt")
      (trim)
      (split #"\n")
      (->> (map parse)
           (into {}))))

(def messages
  "This is the input of the messages"
  (-> (slurp "resources/2020/input/day19msgs.txt")
      (trim)
      (split #"\n")))

(def test-rules
  "Test data for the first part."
  (-> ["0: 4 1 5"
       "1: 2 3 | 3 2"
       "2: 4 4 | 5 5"
       "3: 4 5 | 5 4"
       "4: \"a\""
       "5: \"b\""]
      (->> (map parse)
           (into {}))))

(def test-msgs
  "Test messages to be used with the test rules."
  ["ababbb"
   "bababa"
   "abbbab"
   "aaabbb"
   "aaaabbb"])

(defn fit?
  "Function to see if the provided rule matches the collection, and return
  the remainder of the collection - or nil, if there is no match."
  [r rm coll]
  (cond
    (char? r)
      (if (= r (first coll)) (rest coll))
    (and (< 1 (count r)) (coll? (first r)))
      (some #(fit? % rm coll) r)
    :else
      (loop [rs r
             src coll]
        (if-let [nr (get rm (first rs))]
          (if-let [mre (fit? nr rm src)]
            (recur (rest rs) mre))
          src))))

(defn one
  "Function to count how many of the messages are valid."
  [& [coll]]
  (let [ok? (fn [s] (let [ans (fit? (get rules 0) rules s)]
                      (and (coll? ans) (empty? ans))))]
    (count (filter identity (map ok? messages)))))

(defn two
  "Function to "
  [& [coll]]
  test-rules
  )

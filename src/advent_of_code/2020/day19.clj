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
  "Test messages to be used with the first test rules."
  ["ababbb"
   "bababa"
   "abbbab"
   "aaabbb"
   "aaaabbb"])

(def test-rules-dos
  "Test data for the second part."
  (-> ["42: 9 14 | 10 1"
       "9: 14 27 | 1 26"
       "10: 23 14 | 28 1"
       "1: \"a\""
       "11: 42 31"
       "5: 1 14 | 15 1"
       "19: 14 1 | 14 14"
       "12: 24 14 | 19 1"
       "16: 15 1 | 14 14"
       "31: 14 17 | 1 13"
       "6: 14 14 | 1 14"
       "2: 1 24 | 14 4"
       "0: 8 11"
       "13: 14 3 | 1 12"
       "15: 1 | 14"
       "17: 14 2 | 1 7"
       "23: 25 1 | 22 14"
       "28: 16 1"
       "4: 1 1"
       "20: 14 14 | 1 15"
       "3: 5 14 | 16 1"
       "27: 1 6 | 14 18"
       "14: \"b\""
       "21: 14 1 | 1 14"
       "25: 1 1 | 1 14"
       "22: 14 14"
       "8: 42"
       "26: 14 22 | 1 20"
       "18: 15 15"
       "7: 14 5 | 1 21"
       "24: 14 1"]
      (->> (map parse)
           (into {}))))

(def test-msgs-dos
  "Test messages to be used with the first test rules."
  ["abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
   "bbabbbbaabaabba"
   "babbbbaabbbbbabbbbbbaabaaabaaa"
   "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
   "bbbbbbbaaaabbbbaaabbabaaa"
   "bbbababbbbaaaaaaaabbababaaababaabab"
   "ababaaaaaabaaab"
   "ababaaaaabbbaba"
   "baabbaaaabbaaaababbaababb"
   "abbbbabbbbaaaababbbbbbaaaababb"
   "aaaaabbaabaaaaababaa"
   "aaaabbaaaabbaaa"
   "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
   "babaaabbbaaabaababbaabababaaab"
   "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"])

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

(defn yoyo
  "Function to count how many of the test messages are valid."
  [& [coll]]
  (let [ok? (fn [s] (let [ans (fit? (get test-rules 0) test-rules s)]
                      (and (coll? ans) (empty? ans))))]
    (count (filter identity (map ok? test-msgs)))))

(defn one
  "Function to count how many of the messages are valid."
  [& [coll]]
  (let [ok? (fn [s] (let [ans (fit? (get rules 0) rules s)]
                      (and (coll? ans) (empty? ans))))]
    (count (filter identity (map ok? messages)))))

(defn two
  "Function to "
  [& [coll]]
  (let [nrs (assoc test-rules-dos 8 [[42] [42 8]] 11 [[42 31] [42 11 31]])
        ok? (fn [s] (let [ans (fit? (get nrs 0) nrs s)]
                      (and (coll? ans) (empty? ans))))
       ]
    (count (filter identity (map ok? test-msgs-dos)))
  ))

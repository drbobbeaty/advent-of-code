(ns advent-of-code.2020.day16
  "Sixteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle-fields
  ""
  [{:name "departure location" :limits [[27 180] [187 953]]}
   {:name "departure station"  :limits [[47 527] [545 958]]}
   {:name "departure platform" :limits [[36 566] [572 973]]}
   {:name "departure track"    :limits [[37 497] [505 971]]}
   {:name "departure date"     :limits [[47 707] [719 969]]}
   {:name "departure time"     :limits [[36 275] [290 949]]}
   {:name "arrival location"   :limits [[31 855] [864 955]]}
   {:name "arrival station"    :limits [[50 148] [158 949]]}
   {:name "arrival platform"   :limits [[50 441] [467 965]]}
   {:name "arrival track"      :limits [[30 648] [659 962]]}
   {:name "class"              :limits [[26 470] [481 966]]}
   {:name "duration"           :limits [[27 808] [818 958]]}
   {:name "price"              :limits [[49 769] [784 970]]}
   {:name "route"              :limits [[49 796] [809 964]]}
   {:name "row"                :limits [[42 362] [383 971]]}
   {:name "seat"               :limits [[34 877] [887 952]]}
   {:name "train"              :limits [[31 354] [363 950]]}
   {:name "type"               :limits [[39 208] [231 953]]}
   {:name "wagon"              :limits [[47 736] [746 968]]}
   {:name "zone"               :limits [[44 290] [310 974]]}])

(def puzzle-ticket
  "This is my ticket, and we'll use it later."
  [97 61 53 101 131 163 79 103 67 127 71 109 89 107 83 73 113 59 137 139])

(def puzzle-nearby
  "This is the input of all the nearby tickets that we saw."
  (-> (slurp "resources/2020/input/day16nearby.txt")
      (trim)
      (split "\n")
      (->> (map #(map parse-int (split % ","))))))

(def puzzle
  "This is the final composition of the puzzle data of all the tickets and
  fields that I got from the input."
  {:fields puzzle-fields :my-ticket puzzle-ticket :nearby puzzle-nearby})

(def test1
  "Test data for the first part."
  {:fields [{:name "class" :limits [[1 3] [5 7]]}
            {:name "row"   :limits [[6 11] [33 44]]}
            {:name "seat"  :limits [[13 40] [45 50]]}]
   :my-ticket [7 1 14]
   :nearby [[7 3 47] [40 4 50] [55 2 20] [38 6 12]]})

(def test2
  "Test data for the second part."
  {:fields [{:name "class" :limits [[0 1] [4 19]]}
            {:name "row"   :limits [[0 5] [8 19]]}
            {:name "seat"  :limits [[0 13] [16 19]]}]
   :my-ticket [11 12 13]
   :nearby [[3 9 18] [15 1 5] [5 14 9]]})

(defn invalid
  "Function to see if the provided ticket has any completely invalid nubers -
  those that don't fit into any of the allowed bands on *any* of the fields.
  If so, return them in a sequence, as they are clearly invalid."
  [flds tkt]
  (not-empty
    (for [n tkt
          :when (not (some (fn [[l h]] (<= l n h)) (apply concat (map :limits flds))))]
      n)))

(defn one
  "Function to return the sum of all invalid numbers on any of the tickets."
  [& [m]]
  (let [inp (or m puzzle)]
    (->> (for [n (:nearby inp)] (invalid (:fields inp) n))
         (apply concat)
         (sum))))

(defn possible?
  "Predicate function to return true if the field is valid for all ticket
  values in that position - the first argument."
  [coll fld]
  (let [ok? (fn [n] (some (fn [[l h]] (<= l n h)) (:limits fld)))]
    (every? ok? coll)))

(defn two
  "Function to exclude the bad tickets with 'invalid', and then use the data
  on the fields to find out which fields are which - based on the process of
  elimination on the fields. Find the first, remove it from possibility for
  the others, and continue. Then get the answer."
  [& [m]]
  (let [inp (or m puzzle)
        tkts (remove #(invalid (:fields inp) %) (:nearby inp))
        cols (apply map vector (:my-ticket inp) tkts)
        poss (map-indexed vector
               (for [c cols] [c (filter #(possible? c %) (:fields inp))]))
        cnt (count poss)
        ans (atom {})
       ]
    (loop [src poss]
      (doseq [[i [c fs]] poss
              :let [hit (set (keys @ans))
                    rfs (remove #(hit (:name %)) fs)]]
        (if (= 1 (count rfs))
          (swap! ans assoc (:name (first rfs)) i)))
      (if (= cnt (count @ans))
        @ans
        (recur src)))
    ;; ans has the mappings now - so solve the puzzle question
    (apply *
      (for [[k v] @ans
            :when (.startsWith k "departure")]
        (nth (:my-ticket inp) v)))))

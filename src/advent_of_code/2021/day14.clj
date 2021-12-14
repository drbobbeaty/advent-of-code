(ns advent-of-code.2021.day14
  "Fourteenth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum median]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take the mixed lines from the sub manual, and parse out
  the poylmer template and the pair insertion rules, and return these to
  the caller for easier manipulation."
  [coll]
  (let [pairs (->> (drop 2 coll)
                (map #(split % " -> "))
                (into {}))]
    {:template (first coll) :pairs pairs}))

(def puzzle
  "This is the input of the polymer template and the pair expansions that
  can be done on this polymer."
  (-> (slurp "resources/2021/input/day14.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["NNCB"
       ""
       "CH -> B"
       "HH -> N"
       "CB -> H"
       "NH -> C"
       "HB -> C"
       "HC -> B"
       "HN -> C"
       "NN -> C"
       "BH -> H"
       "NC -> B"
       "NB -> B"
       "BN -> B"
       "BB -> N"
       "BC -> B"
       "CC -> N"
       "CN -> C"]
    (parse)))

(defn pass
  "Function to process the polymer insertion through one pass - just one."
  [poly pairs]
  (let [raw (for [[a b] (partition 2 1 poly)] [a (get pairs (str a b)) b])]
    (->> (map rest (rest raw))
      (apply concat (first raw))
      (flatten)
      (apply str))))

(defn one
  "Function to find the difference in the most common, and least common
  elements in the poylmer chain after 10 passes of the insertion scheme."
  [& [coll]]
  (let [{template :template pairs :pairs} puzzle]
    (loop [poly template
           cnt 10]
      (if (< 0 cnt)
        (recur (pass poly pairs) (dec cnt))
        (let [hits (frequencies poly)
              big (apply max (vals hits))
              lil (apply min (vals hits))]
          (assoc hits :score (- big lil)))))))

(defn evolve
  "Function to execute one evolution of the pairs of characters in the
  polymer. The 'grps' keys are strings of two characters, and the values
  are the count of their occurrence in the polymer. We will then use each
  pair to expand into a similar number of 'spawned' pairs, and then sum
  all them up and return a map very much like 'grps', but for the next
  generation."
  [grps pairs]
  (let [ans (atom {})]
    (doseq [[[a b] v] grps
            :let [c (get pairs (str a b))
                  fp (str a c)
                  sp (str c b)]]
      (swap! ans assoc fp (+ (get @ans fp 0) v))
      (swap! ans assoc sp (+ (get @ans sp 0) v)))
    @ans))

(defn two
  "Function to find the difference in the most common, and least common
  elements in the poylmer chain after 40 passes of the insertion scheme.
  As it turns out, this also works for part 1, but we left that as a way
  to remmeber how it was on the first pass."
  [& [coll]]
  (let [{template :template pairs :pairs} puzzle
        src (into {} (for [[[a b] v] (frequencies (partition 2 1 template))]
              [(str a b) v]))]
    (loop [pops src
           cnt 40]
      (if (< 0 cnt)
        (recur (evolve pops pairs) (dec cnt))
        (let [hits (atom {(first template) 1})]
          ;; update all the stats for the character usage in the answer
          (doseq [[c v] (for [[[_ b] v] pops] [b v])]
            (swap! hits assoc c (+ (get @hits c 0) v)))
          ;; now compute the score based on the most and least occurring chars
          (let [big (apply max (vals @hits))
                lil (apply min (vals @hits))]
            (assoc @hits :score (- big lil))))))))

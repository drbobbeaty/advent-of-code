(ns advent-of-code.2017.day07
  "Seventh day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int sum]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(defn code-it
  "Function to parse the data from the puzzle input into a usable format for
  processing."
  [s]
  (cond
    (string? s) (if (cs/includes? s "->")
                  (let [[nme wgt lst] (drop 1 (re-matches #"^(\S+) \((\d+)\) -> ([\S ,]+)$" s))]
                    {:name nme :weight (parse-int wgt) :children (cs/split lst #", ")})
                  (let [[nme wgt] (drop 1 (re-matches #"^(\S+) \((\d+)\)$" s))]
                    {:name nme :weight (parse-int wgt)}))
    (coll? s)   (map code-it s)
    :else       s))

(def puzzle
  "This is the structure of the tower of programs, with weights, that comes
  from the puzzle input."
  (->> "resources/2017/input/day07.txt"
    (io/reader)
    (line-seq)
    (code-it)))

(def sample
  "This is the example data in the puzzle."
  (->> ["pbga (66)"
        "xhth (57)"
        "ebii (61)"
        "havc (66)"
        "ktlj (57)"
        "fwft (72) -> ktlj, cntj, xhth"
        "qoyq (66)"
        "padx (45) -> pbga, havc, qoyq"
        "tknk (41) -> ugml, padx, fwft"
        "jptl (61)"
        "ugml (68) -> gyxo, ebii, jptl"
        "gyxo (61)"
        "cntj (57)"]
    (code-it)))

(defn chk-wgts
  "Function to fill in the child weights for each node in the graph. This will
  loop until all the weights are filled in properly. This is done to make all
  the weights 'ripple up' as necessary."
  [s]
  (loop [nl s]
    (let [nxt (for [pgm nl]
                (if (:children pgm)
                  (let [cw (for [cn (:children pgm)
                                 :let [nde (first (filter #(= cn (:name %)) nl))]]
                             (or (:total nde) (:weight nde)))]
                    (assoc pgm :child-wgts cw :total (+ (sum cw) (:weight pgm))))
                  pgm))]
      (if-not (= nxt nl)
        (recur nxt)
        nxt))))

(defn one
  "Function to look at the tree structure of the puzzle, and find out the
  name of the program that is the parent (root) of them all. This is pretty
  easy by just makeing all the names a set, and removing the names of the
  children."
  []
  (let [all (atom (set (map :name puzzle)))]
    (doseq [p (filter :children puzzle)
            nme (:children p)]
      (swap! all disj nme))
    (first @all)))

(defn two
  "Function to find the highest node with a misbalance, and then let me know
  what the imbalance is, so I can answer the question as to how to correct
  it."
  []
  (let [yank (fn [n] (first (filter #(= n (:name %)) puzzle)))
        nds (chk-wgts puzzle)
        prob (->> (filter :child-wgts nds)
                    (remove #(apply = (:child-wgts %)))
                    (sort-by :total)
                    (first))
        kids (map vector (:children prob) (:child-wgts prob))
        {odd :odd norm :norm} (into {}
                                (for [[k v] (frequencies (map second kids))]
                                  (if (= 1 v) [:odd k] [:norm k])))
        oddn (first (first (filter #(= odd (second %)) kids)))]
    (assoc (first (filter #(= oddn (:name %)) nds)) :norm norm)))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (let [yank (fn [n] (first (filter #(= n (:name %)) puzzle)))
        nds (chk-wgts puzzle)
        prob (->> (filter :child-wgts nds)
                    (remove #(apply = (:child-wgts %)))
                    (sort-by :total)
                    (first))
        kids (map vector (:children prob) (:child-wgts prob))
        {odd :odd norm :norm} (into {}
                                (for [[k v] (frequencies (map second kids))]
                                  (if (= 1 v) [:odd k] [:norm k])))
        oddn (first (first (filter #(= odd (second %)) kids)))]
    (assoc (first (filter #(= oddn (:name %)) nds)) :norm norm)))

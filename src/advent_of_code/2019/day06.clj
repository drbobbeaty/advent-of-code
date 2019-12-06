(ns advent-of-code.2019.day06
  "Sixth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of opbital patterns for each pair of ships and such."
  (-> (slurp "resources/2019/input/day06.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map #(cs/split % #"\)")))))

(def trial1
  "Test data for the first part of the puzzle"
  [["COM" "B"]
   ["B" "C"]
   ["C" "D"]
   ["D" "E"]
   ["E" "F"]
   ["B" "G"]
   ["G" "H"]
   ["D" "I"]
   ["E" "J"]
   ["J" "K"]
   ["K" "L"]])

(def trial2
  "Test data for the second part of the puzzle today"
  (concat trial1 [["K" "YOU"] ["I" "SAN"]]))

(defn- mk-kids
  "Function to take the orbital pair data anc convert it into a map of all
  the parent/child relationships in the data. This makes it a lot easier to
  know the relationships with each thing being a key."
  [s]
  (loop [os s
         ans {}]
    (if-let [[oa ob] (first os)]
      (recur (rest os) (if (contains? ans oa)
                         (update ans oa conj ob)
                         (assoc ans oa [ob])))
      ans)))

(defn- mk-steps
  "Function to make a nested list of all nodes, and the distance they have
  from the provided key - based on the map data as a representation of the
  parent/child relationships."
  [m k & [cnt]]
  (for [ck (get m k)] (conj (mk-steps m ck (inc (or cnt 1))) [ck (or cnt 1)])))

(defn- climb
  "Function to start at a node in the tree, and climb all the way to the
  root, adding steps to the path as we climb."
  [m bdy]
  (loop [cb bdy
         ans (transient [])]
    (if-let [nb (first (for [[k v] m :when (<= 0 (.indexOf v cb))] k))]
      (recur nb (conj! ans nb))
      (persistent! ans))))

(defn one
  "Function to count all the direct and indirect orbits for the data."
  [& coll]
  (let [kids (mk-kids (or coll puzzle))]
    (->> (mk-steps kids "COM")
         (flatten)
         (filter number?)
         (apply +))))

(defn two
  "Function to count the number of orbital transitions needed to take to
  get from YOU to SAN in the provided orbital data. This doesn't include
  me or Santa, just the number of transitions."
  [& coll]
  (let [kids (mk-kids (or coll puzzle))
        you (climb kids "YOU")
        san (climb kids "SAN")
        shd (->> (map vector (reverse you) (reverse san))
                 (filter #(= (first %) (second %)))
                 (count))]
    (+ (count (drop-last shd you)) (count (drop-last shd san)))))

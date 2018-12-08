(ns advent-of-code.2018.day08
  "Eighth day's solutions for the Advent of Code 2018"
  (require [clojure.java.io :as io]
           [clojure.string :as cs])
  (import [java.util LinkedList]))

(def puzzle
  "This is the "
  (-> (slurp "resources/2018/input/day08.txt")
    (cs/trim)
    (cs/split #" ")
    (as-> s (map #(Integer/parseInt (cs/trim %)) s))))

(defn eat
  "Function like `take`, but removes the elements from the sequence with the
  `pop` function on the list so that we have less in the source as well."
  [n s]
  (let [r (atom [])]
    (doseq [i (range n)]
      (swap! r conj (.pop s)))
    @r))

(defn mknode
  "Function to take a LinkedList of numbers and create the node data structure
  that the puzzle describes where each node is of the form:
    {:children [n1 n2 n3]
     :metadata [1 2 3]}
  where n1, n2, n3 are all nodes themselves."
  [s]
  (let [qcn (.pop s)
        qme (.pop s)]
  {:children (doall (for [i (range qcn)] (mknode s)))
   :metadata (eat qme s)}))

(defn md-sum
  "Function to sum all the metadata in a simple recursive manner given the
  node map."
  [m]
  (apply + (concat (:metadata m) (for [c (:children m)] (md-sum c)))))

(defn md-sum-c
  "Function to sum all the metadata in a simple recursive manner given the
  node map."
  [{cs :children ms :metadata :as arg}]
  (if (= 0 (count cs))
    (apply + ms)
    (apply + (for [idx ms] (md-sum-c (nth cs (dec idx) 0))))))

(defn one
  "Function to compute the sum of the metadata for all the nodes in the
  input - once the input has been converted into a node structure with
  children and metadata."
  [& [src]]
  (let [sls (LinkedList. (or src puzzle))]
    (md-sum (mknode sls))))

(defn two
  "Function to compute the sum of the metadata for all the nodes in the
  input - once the input has been converted into a node structure with
  children and metadata."
  [& [src]]
  (let [sls (LinkedList. (or src puzzle))]
    (md-sum-c (mknode sls))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [sls (LinkedList. [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
        ts (mknode sls)]
    (md-sum-c ts)))

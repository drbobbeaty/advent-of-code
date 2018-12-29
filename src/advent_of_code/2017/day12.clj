(ns advent-of-code.2017.day12
  "Twelveth day's solutions for the Advent of Code 2017"
  (require [advent-of-code.util :refer [parse-int]]
           [clojure.java.io :as io]
           [clojure.set :refer [difference]]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf
                                          debug debugf]]))

(defn code-it
  "Function to parse the input from the puzzle into something that's reasonable
  to work with."
  [s]
  (cond
    (string? s) (let [[src dest] (drop 1 (re-matches #"^(\d+) <-> (.+)$" s))
                      dlst (mapv parse-int (cs/split dest #", "))]
                  [(parse-int src) dlst])
    (coll? s)   (into {} (map code-it s))
    :else       s))

(def puzzle
  "These are the channels of communication from one program to another. Each
  is a simple 1:many map, and we need to use this to discover the networks
  in the population."
  (-> (slurp "resources/2017/input/day12.txt")
    (cs/trim)
    (cs/split #"\n")
    (code-it)))

(def sample
  "This is the sample data from the puzzle."
  (-> ["0 <-> 2"
       "1 <-> 1"
       "2 <-> 0, 3, 4"
       "3 <-> 2, 4"
       "4 <-> 2, 3, 6"
       "5 <-> 6"
       "6 <-> 4, 5"]
    (code-it)))

(defn talkers
  "Function to return the collection of all programs that can talk to the
  provided target ID, and whose network graph is specified in the map."
  [net tgt]
  (loop [ks [tgt]
         hits #{tgt}]
    (if-let [k (first ks)]
      (let [vs (remove hits (get net k))]
        (recur (apply conj (rest ks) vs) (apply conj hits vs)))
      hits)))

(defn groups
  "Function to take a description of the programs that can talk to one
  another, return a collection of all the groups that can talk to each
  other. This will include all the programs - not just any one target."
  [net]
  (loop [ks (set (keys net))
         grps []]
    (if-let [k (first ks)]
      (let [tks (talkers net k)]
        (recur (difference ks tks) (conj grps tks)))
      grps)))

(defn one
  "Function to return the number of programs that can talk to #0 - based on
  the puzzle definition."
  []
  (count (talkers puzzle 0)))

(defn two
  ""
  []
  (groups puzzle))

(defn yoyo
  "This is just a function I use to see if the components are working."
  []
  (let [net sample]
    (loop [ks (set (keys net))
           grps []]
      (if-let [k (first ks)]
        (let [tks (talkers net k)]
          (recur (difference ks tks) (conj grps tks)))
        grps))))

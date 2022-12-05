(ns advent-of-code.2022.day05
  "Fifth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :refer [includes?]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function that takes the input and parses it into the arrangement of
  crates in the stacks, and the moves the crane operator needs to make."
  [coll]
  (let [moves (atom [])
        crates (atom {})
        do-move (fn [s] (let [[cnt fr to] (rest (map parse-int (re-matches #"move (\d+) from (\d+) to (\d+)" s)))]
                  {:move cnt :from fr :to to}))]
    ;; get the moves first, as we need to know the number of stacks
    (doseq [l (filter #(.startsWith % "move") coll)]
      (swap! moves conj (do-move l)))
    ;; now build up the stacks of the crates
    (let [cnt (apply max (concat (map :from @moves) (map :to @moves)))
          pos (map (fn [i] [(inc i) (inc (* 4 i))]) (range cnt))]
      (doseq [l (filter #(includes? % "[") coll)
              [i p] pos
              :let [k (keyword (str i))
                    c (nth l p \space)]
              :when (not= c \space)]
        (swap! crates assoc k (conj (k @crates) c))))
    {:crates @crates :moves @moves}))

(def puzzle
  "This is the input of the crate stacks and crane operator movements."
  (-> (slurp "resources/2022/input/day05.txt")
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> ["    [D]    "
       "[N] [C]    "
       "[Z] [M] [P]"
       " 1   2   3 "
       ""
       "move 1 from 2 to 1"
       "move 3 from 1 to 3"
       "move 2 from 2 to 1"
       "move 1 from 1 to 2"]
    (parse)))

(defn singles
  "Function to move one crate at a time, but complete the 'step' starting
  with the stack of crates. This is due to a limitation in the crane of doing
  just one at a time."
  [crates step]
  (loop [stacks crates
         cnt (:move step)]
    (if (= 0 cnt)
      stacks
      (let [fk (keyword (str (:from step)))
            tk (keyword (str (:to step)))
            newf (butlast (fk stacks))
            newt (concat (tk stacks) [(last (fk stacks))])]
        (recur (assoc stacks fk newf tk newt) (dec cnt))))))

(defn one
  "Function to find the top crates on each stack after all the operations
  are complete."
  [& [coll]]
  (let [stacks (loop [crates (:crates puzzle)
                      moves (:moves puzzle)]
                 (if-let [mv (first moves)]
                   (recur (singles crates mv) (rest moves))
                   crates))]
    (apply str
      (for [i (map inc (range (count stacks)))
            :let [k (keyword (str i))]]
        (last (k stacks))))))

(defn multis
  "Function to move all crates in one movement because this crane is able
  to move as many as needed, and keep them in the order they started in."
  [crates step]
  (let [fk (keyword (str (:from step)))
        tk (keyword (str (:to step)))
        cnt (:move step)
        newf (drop-last cnt (fk crates))
        newt (concat (tk crates) (take-last cnt (fk crates)))]
    (assoc crates fk newf tk newt)))

(defn two
  "Function to find the top crates on each stack after all the operations
  are complete - but with the multi-crate moving crane."
  [& [coll]]
  (let [stacks (loop [crates (:crates puzzle)
                      moves (:moves puzzle)]
                 (if-let [mv (first moves)]
                   (recur (multis crates mv) (rest moves))
                   crates))]
    (apply str
      (for [i (map inc (range (count stacks)))
            :let [k (keyword (str i))]]
        (last (k stacks))))))

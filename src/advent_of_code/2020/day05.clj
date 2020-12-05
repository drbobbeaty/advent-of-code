(ns advent-of-code.2020.day05
  "Fifth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the boarding passes that are scanned by my phone."
  (-> (slurp "resources/2020/input/day05.txt")
      (cs/trim)
      (cs/split #"\n")))

(def test1
  "Test data for the first part."
  ["BFFFBBFRRR"
   "FFFBBBFRRR"
   "BBFFBBFRLL"])

(defn search
  "Function to search a space, with a binary search, based on the characters
  provided in the string arg. The size of the starting span is dictated by the
  length of the moves it takes to come to a single value at the end."
  [arg]
  (loop [src (seq arg)
         l 0
         h (if (= 7 (count arg)) 127 7)]
    (if-let [dir (first src)]
      (let [jmp (/ (inc (- h l)) 2)
            [nl nh] (if (#{\F \L} dir) [l (- h jmp)] [(+ l jmp) h])]
        (recur (rest src) nl nh))
      l)))

(defn locate
  "Function to take a boarding pass, and using 'search', find the row and col
  of the seat location, and then compute the seat ID, and put them all in a
  map to return to the caller."
  [pass]
  (let [row (search (subs pass 0 7))
        col (search (subs pass 7))]
    {:row row :col col :id (+ (* 8 row) col)}))

(defn one
  "Function to search all the boarding passes in the input and find the one
  with the highest valued seat ID."
  [& [coll]]
  (last (sort-by :id (map locate (or coll puzzle)))))

(defn two
  "Function to find my seat ID by looking at all the possible seats in the
  boarding pass list, and find out which one is missing. That's me."
  [& [coll]]
  (let [seats (map locate (or coll puzzle))
        filled (set (map :id seats))
        l (apply min filled)
        h (apply max filled)]
    (for [i (range l (inc h)) :when (not (filled i))]
      i)))

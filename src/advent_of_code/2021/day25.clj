(ns advent-of-code.2021.day25
  "Twenty-fifth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum transpose]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the locations of all the sea cucumbers on the sea floor."
  (-> (slurp "resources/2021/input/day25.txt")
    (trim)
    (split #"\n")))

(def test1
  "Test data for the first part."
  ["...>..."
   "......."
   "......>"
   "v.....>"
   "......>"
   "......."
   "..vvv.."])

(def test2
  "Test data for the first part."
  ["v...>>.vv>"
   ".vv>>.vv.."
   ">>.>v>...v"
   ">>v>>.>.v."
   "v>v.vv.v.."
   ">.>>..v..."
   ".vv..>.>v."
   "v.v..>>v.v"
   "....v..v.>"])

(defn move
  "Function to look at the movement *across* a row for the sea cucubmber with
  the designator 'cuc'. If there is an open spot along the row, and that
  includes wrapping around the end, then do it. If not, don't."
  [sea cuc]
  (let [rcnt (count sea)]
    (for [r (range rcnt) :let [row (nth sea r)]]
      (apply str
        (for [[l m r] (partition 3 1 (str (last row) row (first row)))]
          (cond
            (and (= l cuc) (= m \.)) cuc
            (and (= m cuc) (= r \.)) \.
            :else m))))))

(defn east
  "Function to move all of the easterly-moving sea cucumbers one step."
  [sea]
  (move sea \>))

(defn south
  "Function to move all of the southerly-moving sea cucumbers one step."
  [sea]
  (let [tsp (map #(apply str %) (transpose sea))]
    (map #(apply str %) (transpose (move tsp \v)))))

(defn one
  "Function to find the number of steps it takes for the sea cucumbers to
  reach a stable configuration."
  [& [coll]]
  (loop [sea puzzle
         stp 0]
    (let [nxt (south (east sea))
          nstp (inc stp)]
      (if (= nxt sea)
        {:steps nstp}
        (recur nxt nstp)))))

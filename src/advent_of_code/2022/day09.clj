(ns advent-of-code.2022.day09
  "Ninth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to turn the direction, steps into a series of delts tuples
  for the movement of the head. This just removes one loop on the processing."
  [s]
  (let [dir (case (first s)
              \R [1 0]
              \L [-1 0]
              \U [0 1]
              \D [0 -1])
        cnt (parse-int (subs s 2))]
    (repeat cnt dir)))

(def puzzle
  "This is the input of the moves for the head of the rope."
  (-> (slurp "resources/2022/input/day09.txt")
    (trim)
    (split #"\n")
    (->> (map parse)
      (apply concat))))

(def test1
  "Test data for the first part."
  (-> ["R 4"
       "U 4"
       "L 3"
       "D 1"
       "R 4"
       "D 1"
       "L 5"
       "R 2"]
    (->> (map parse)
      (apply concat))))

(defn tmove
  "Function to take head and tail, or one knot, and the next not, and
  return where the new position of the second knot should be. This is
  just dealing with all the possibilities of where the knot should go,
  given it's location and the location of the knot ahead of it."
  [[hx hy] [tx ty]]
  (cond
    (and (= (+ tx 2) hx) (= ty hy))       [(inc tx) ty]
    (and (= (+ tx 1) hx) (= (+ ty 2) hy)) [(inc tx) (inc ty)]
    (and (= (+ tx 2) hx) (= (+ ty 1) hy)) [(inc tx) (inc ty)]
    (and (= (+ tx 2) hx) (= (+ ty 2) hy)) [(inc tx) (inc ty)]
    (and (= tx hx) (= (+ ty 2) hy))       [tx (inc ty)]
    (and (= (- tx 1) hx) (= (+ ty 2) hy)) [(dec tx) (inc ty)]
    (and (= (- tx 2) hx) (= (+ ty 1) hy)) [(dec tx) (inc ty)]
    (and (= (- tx 2) hx) (= (+ ty 2) hy)) [(dec tx) (inc ty)]
    (and (= (- tx 2) hx) (= ty hy))       [(dec tx) ty]
    (and (= (- tx 1) hx) (= (- ty 2) hy)) [(dec tx) (dec ty)]
    (and (= (- tx 2) hx) (= (- ty 1) hy)) [(dec tx) (dec ty)]
    (and (= (- tx 2) hx) (= (- ty 2) hy)) [(dec tx) (dec ty)]
    (and (= tx hx) (= (- ty 2) hy))       [tx (dec ty)]
    (and (= (+ tx 1) hx) (= (- ty 2) hy)) [(inc tx) (dec ty)]
    (and (= (+ tx 2) hx) (= (- ty 1) hy)) [(inc tx) (dec ty)]
    (and (= (+ tx 2) hx) (= (- ty 2) hy)) [(inc tx) (dec ty)]
    :else                                 [tx ty]))

(defn one
  "Function to find the number of unique spaces that the tail occupies
  as the head is moved based on the motion in the puzzle."
  [& [coll]]
  (loop [[hx hy] [0 0]
         [tx ty] [0 0]
         tvisits [[0 0]]
         hmoves puzzle]
    (if-let [[dx dy] (first hmoves)]
      (let [[nhx nhy] [(+ hx dx) (+ hy dy)]
            nt (tmove [nhx nhy] [tx ty])]
        (recur [nhx nhy] nt (conj tvisits nt) (rest hmoves)))
      (count (distinct tvisits)))))

(def test2
  "Test data for the second part."
  (-> ["R 5"
       "U 8"
       "L 8"
       "D 3"
       "R 17"
       "D 10"
       "L 25"
       "U 20"]
    (->> (map parse)
      (apply concat))))

(defn two
  "Function to find the number of unique spaces that the tail occupies
  as the head is moved based on the motion in the puzzle - but with
  eight knots in between the head and tail."
  [& [coll]]
  (loop [[hx hy] [0 0]
         s1 [0 0]
         s2 [0 0]
         s3 [0 0]
         s4 [0 0]
         s5 [0 0]
         s6 [0 0]
         s7 [0 0]
         s8 [0 0]
         t [0 0]
         tvisits [[0 0]]
         hmoves puzzle]
    (if-let [[dx dy] (first hmoves)]
      (let [[nhx nhy] [(+ hx dx) (+ hy dy)]
            ns1 (tmove [nhx nhy] s1)
            ns2 (tmove ns1 s2)
            ns3 (tmove ns2 s3)
            ns4 (tmove ns3 s4)
            ns5 (tmove ns4 s5)
            ns6 (tmove ns5 s6)
            ns7 (tmove ns6 s7)
            ns8 (tmove ns7 s8)
            nt (tmove ns8 t)]
        (recur [nhx nhy] ns1 ns2 ns3 ns4 ns5 ns6 ns7 ns8 nt (conj tvisits nt) (rest hmoves)))
      (count (distinct tvisits)))))

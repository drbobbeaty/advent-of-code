(ns advent-of-code.2022.day13
  "Thirteenth day's solutions for the Advent of Code 2022"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Parse all the packet data into pairs with an index for easy use."
  [coll]
  (loop [src coll
         idx 1
         data []]
    (let [[l r] (take 2 src)]
      (if (< 2 (count src))
        (recur (drop 3 src) (inc idx) (conj data [idx (read-string l) (read-string r)]))
        (conj data [idx (read-string l) (read-string r)])))))

(def puzzle
  "This is the input of all the packets from the distress signal"
  (-> (slurp "resources/2022/input/day13.txt")
    (trim)
    (split #"\n")
    (parse)))

(def test1
  "Test data for the first part."
  (-> (slurp "resources/2022/input/day13test.txt")
    (trim)
    (split #"\n")
    (parse)))

(defn order
  "Function to look at two packets and see if they are in the right order.
  The return value will be: -1 is fail and stop, 0 is keep going,
  1 is true and stop."
  [l r]
  (cond
    (and (number? l) (number? r))
      (cond
        (< l r) 1
        (= l r) 0
        (> l r) -1)
    (and (number? l) (vector? r))
      (order [l] r)
    (and (vector? l) (number? r))
      (order l [r])
    :else
      (loop [srcl l
             srcr r]
        (let [tl (first srcl)
              tr (first srcr)]
          (cond
            (and (nil? tl) tr) 1
            (and (nil? tl) (nil? tr)) 0
            (and tl (nil? tr)) -1
            :else
              (let [chk (order tl tr)]
                (if (zero? chk)
                  (recur (rest srcl) (rest srcr))
                  chk)))))))

(defn one
  "Function to find the sum of all indeces of the pairs that are in the
  right order."
  [& [coll]]
  (sum
    (for [[i l r] puzzle
          :when (<= 0 (order l r))]
      i)))

(defn two
  "Function to find the decoder key by leveraging order and using sort."
  [& [coll]]
  (let [lst (->> puzzle
              (map rest)
              (apply concat [[[2]] [[6]]])
              (sort order)
              (reverse))
        two (inc (.indexOf lst [[2]]))
        six (inc (.indexOf lst [[6]]))
       ]
    (* two six)))

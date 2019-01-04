(ns advent-of-code.2017.day25
  "Twenty-fifth day's solution for the Advent of Code 2017"
  (:require [advent-of-code.util :refer [parse-int sum split rev]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf
                                           debug debugf]]))

(def puzzle
  "This is the list of components that the puzzle has."
  {:a [{:write 1 :move 1 :next :b}  {:write 0 :move 1 :next :f}]
   :b [{:write 0 :move -1 :next :b} {:write 1 :move -1 :next :c}]
   :c [{:write 1 :move -1 :next :d} {:write 0 :move 1 :next :c}]
   :d [{:write 1 :move -1 :next :e} {:write 1 :move 1 :next :a}]
   :e [{:write 1 :move -1 :next :f} {:write 0 :move -1 :next :d}]
   :f [{:write 1 :move 1 :next :a} {:write 0 :move -1 :next :e}]}
   )

(def sample
  "This is the sample list of components used in the puzzle."
  {:a [{:write 1 :move 1 :next :b}  {:write 0 :move -1 :next :b}]
   :b [{:write 1 :move -1 :next :a} {:write 1 :move 1 :next :a}]})

(defn one
  "Function to run the Turing machine for a set numbers of steps, and then
  compute the 'checksum' - which is just the sum of the digits on the tape."
  []
  (let [lim 12425180
        fsm puzzle]
    (loop [tpe (vec (repeat 100000 0))
           pos (quot 100000 2)
           sta :a
           cnt 0]
      (if (< cnt lim)
        (let [tv (nth tpe pos)
              {wr :write mv :move nx :next :as ins} (nth (sta fsm) tv)]
          (recur (assoc tpe pos wr) (+ mv pos) nx (inc cnt)))
        {:steps cnt :pos pos :chk (sum tpe)}))))

(defn yoyo
  "Function to test the components as we are building up things. That's it."
  []
  (let [lim 6
        fsm sample]
    (loop [tpe (vec (repeat 1000 0))
           pos 500
           sta :a
           cnt 0]
      (if (< cnt lim)
        (let [tv (nth tpe pos)
              {wr :write mv :move nx :next :as ins} (nth (sta fsm) tv)]
          (recur (assoc tpe pos wr) (+ mv pos) nx (inc cnt)))
        {:steps cnt :state sta :pos pos :chk (sum tpe)}))))

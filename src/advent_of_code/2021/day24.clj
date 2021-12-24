(ns advent-of-code.2021.day24
  "Twenty-fourth day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split sum hr-in-millis]]
            [clojure.core.memoize :as memo]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a line of the program, and parse it into a useful data
  structure for easier execution."
  [s]
  (let [[op a b] (split s " ")
        a (if (<= 0 (.indexOf "wxyz" a)) (keyword a) (parse-int a))
        b (if b (if (<= 0 (.indexOf "wxyz" b)) (keyword b) (parse-int b)))
       ]
    {:op (keyword op) :a a :b b}
  ))

(def puzzle
  "This is the input of the MONAD boot program."
  (-> (slurp "resources/2021/input/day24.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (->> ["inp z"
        "inp x"
        "mul z 3"
        "eql z x"]
    (map parse)))

(defn run
  "Function to run the program 'pgm', with the initial state 'cpu', and the
  input set 'inp', and return the final cpu state - including all registers."
  [cpu pgm inp]
  (infof "%s" (apply str inp))
  (loop [state cpu
         idata inp]
    (if (<= (count pgm) (:pc state))
      {:cpu state :inp idata}
      (let [pc (:pc state)
            {op :op sa :a sb :b :as inst} (nth pgm pc)
            a (if (keyword? sa) (sa state) sa)
            b (if (keyword? sb) (sb state) sb)]
        (case op
          :inp
            (recur (assoc state sa (first idata) :pc (inc pc)) (rest idata))
          :add
            (recur (assoc state sa (+ a b) :pc (inc pc)) idata)
          :mul
            (recur (assoc state sa (* a b) :pc (inc pc)) idata)
          :div
            (recur (assoc state sa (quot a b) :pc (inc pc)) idata)
          :mod
            (recur (assoc state sa (mod a b) :pc (inc pc)) idata)
          :eql
            (recur (assoc state sa (if (= a b) 1 0) :pc (inc pc)) idata))))))

;;
;; Based on the run-through of the code - highlighted in day24-algo.txt,
;; there turns out to be constraints on the digits d1, d2, ... d14 so that
;; the final value of the z register is zero. The Walkthrough explains all
;; this, but the constraints for a resulting zero value in the z register
;; are:
;;
;;   (d4-5) = d5
;;   (d7-4) = d8
;;   (d9+2) = d10
;;   (d6+8) = d11
;;   (d3+5) = d12
;;   (d2-2) = d13
;;   (d1+6) = d14
;;
;; Which means that the pairs of digits have to be:
;;
;;   d4: [6 7 8 9] & d5: [1 2 3 4]
;;   d7: [5 6 7 8 9] & d8: [1 2 3 4 5]
;;   d9: [1 2 3 4 5 6 7] & d10: [3 4 5 6 7 8 9]
;;   d6: [1] & d11: [9]
;;   d3: [1 2 3 4] & d12: [6 7 8 9]
;;   d2: [3 4 5 6 7 8 9] & d13: [1 2 3 4 5 6 7]
;;   d1: [1 2 3] & d14: [7 8 9]
;;

(defn one
  "Function to find the largest model number that is valid."
  [& [coll]]
  (run {:w 0 :x 0 :y 0 :z 0 :pc 0} puzzle [3 9 4 9 4 1 9 5 7 9 9 9 7 9]))

(defn two
  "Function to find the smallest model number that is valid."
  [& [coll]]
  (run {:w 0 :x 0 :y 0 :z 0 :pc 0} puzzle [1 3 1 6 1 1 5 1 1 3 9 6 1 7]))

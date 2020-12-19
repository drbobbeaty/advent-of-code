(ns advent-of-code.2020.day18
  "Eighteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum un-seq]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn fix
  "Function to put spaces around parems so they can be treated as separate
  tokens in the parsing."
  [s]
  (-> s
      (cs/replace #"\(" " ( ")
      (cs/replace #"\)" " ) ")
      (split " ")
      (->> (remove empty?))))

(def puzzle
  "This is the input of the sea port's computer system."
  (-> (slurp "resources/2020/input/day18.txt")
      (trim)
      (split #"\n")
      (->> (map fix))))

(def test1
  "Test data for the first part."
  (-> ["1 + 2 * 3 + 4 * 5 + 6"
       "1 + (2 * 3) + (4 * (5 + 6))"
       "2 * 3 + (4 * 5)"
       "5 + (8 * 3 + 9 + 3 * 4 * 3)"
       "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
       "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"]
      (->> (map fix))))

(declare calc)

(defn yank
  "Function to parse a value from the head of the list - be it a number or
  the start of a paren expression. If the latter, get the full context of
  the expression - including sub-expressions, and pass it to the provided
  calc function for evaluation."
  [coll cfn]
  (let [ft (first coll)]
    (if (= ft "(")
      (let [[pars mre] (loop [src (rest coll)
                              ans (transient [])
                              dpth 0]
                         (if-let [nt (first src)]
                           (if (and (= ")" nt) (= 0 dpth))
                             [(persistent! ans) (rest src)]
                             (recur (rest src) (conj! ans nt)
                               (case nt
                                 "(" (inc dpth)
                                 ")" (dec dpth)
                                 dpth)))
                           [(persistent! ans) []]))]
        [(cfn pars) mre])
      [(parse-int ft) (rest coll)])))

(defn calc
  "Function to take a sequence of tokens and evaluate them based on the
  rules of the puzzle - operator precedence is left-to-right, but
  respecting parens."
  [coll]
  (loop [toks coll
         acc nil
         op nil]
    (if-let [f (first toks)]
      (cond
        (= f "*") (recur (rest toks) acc *)
        (= f "+") (recur (rest toks) acc +)
        (nil? op) (let [[v rts] (yank toks calc)]
                    (recur rts v nil))
        :else (let [[v rts] (yank toks calc)]
                (recur rts (op acc v) nil)))
      acc)))

(defn one
  "Function to sum the values of all the expressions in the puzzle input."
  [& [coll]]
  (sum (map calc puzzle)))

(defn two
  "Function to "
  [& [coll]]
  )

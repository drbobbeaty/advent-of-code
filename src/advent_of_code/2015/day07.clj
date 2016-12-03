(ns advent-of-code.2015.day07
  (:require [clojure.core.memoize :as memo]
            [clojure.string :as cs]))

(declare probe)

(defn parse-int
  "Function to parse a string into an integer - if possible."
  [x]
  (if (string? x)
    (try
      (Integer/parseInt (cs/trim x))
      (catch java.lang.NumberFormatException nfe))
    x))

(def ^:dynamic *network* nil)

(def puzzle
  "This is the input from the site for the elevator moves for Santa."
  (into {} (for [l (cs/split-lines (slurp "resources/2015/input/day07.txt"))
                 :let [ops #{"AND" "OR" "NOT" "LSHIFT" "RSHIFT"}
                       flip (fn [x] (if (ops x) x (or (parse-int x) (keyword x))))
                       [_ src dest] (re-matches #"(.*?) -> (.*?)" l)
                       sp (mapv flip (.split src " "))]]
             [(keyword dest) sp])))

(defn probe*
  "Function to probe into the design and return the value of the circuit
  provided. This will recurse on itself to get all the values, and then
  return with the final result."
  ([x]
    (cond
      (number? x) x
      (keyword? x) (apply probe (x *network*))))
  ([op x]
    (let [x' (if (number? x) x (probe x))]
      (case op
        "NOT" (bit-xor 65535 x')
        nil)))
  ([x op y]
    (let [x' (if (number? x) x (probe x))
          y' (if (number? y) y (probe y))]
      (case op
        "AND"    (bit-and x' y')
        "OR"     (bit-or x' y')
        "LSHIFT" (bit-and 65535 (bit-shift-left x' y'))
        "RSHIFT" (unsigned-bit-shift-right x' y')))))

(def probe
  "Memoized function to not repeat these calculations if we already have
  them."
  (memo/lru probe* :lru/threshold 1000))

(defmacro with-puzzle
  "Macro to set up the dynamic network variable for the processing."
  [puz & body]
  `(binding [*network* ~puz]
     ~@body))

(defn part1
  "Function to complete part 1"
  []
  (memo/memo-clear! probe)
  (with-puzzle puzzle
    (probe :a)))

(defn part2
  "Function to complete part 2"
  []
  (memo/memo-clear! probe)
  (with-puzzle (assoc puzzle :b [16076])
    (probe :a)))

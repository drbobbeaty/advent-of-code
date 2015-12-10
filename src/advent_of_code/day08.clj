(ns advent-of-code.day08
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the site for the strings for Santa."
  (cs/split-lines (slurp "resources/input/day08.txt")))

(defn esc
  "Function to escape Santa's strings the way he wants them."
  [arg]
  (cond
    (string? arg) (str \" (-> arg
                              (cs/replace "\\" "\\\\")
                              (cs/replace "\"" "\\\"")) \")
    (coll? arg)   (map esc arg)
    :else nil))

(defn code
  "Function to count the 'code' characters in the string, or collection of
  strings."
  [arg]
  (cond
    (string? arg) (count arg)
    (coll? arg)   (apply + (map code arg))
    :else 0))

(defn memory
  "Function to count the 'memory' characters in the string, or collection of
  strings."
  [arg]
  (cond
    (string? arg) (-> arg
                      (.subSequence 1 (dec (count arg)))
                      (cs/replace #"\\x[0-9a-f]{2}" "H")
                      (cs/replace #"\\\\" "S")
                      (cs/replace #"\\\"" "D")
                      (count))
    (coll? arg)   (apply + (map memory arg))
    :else 0))

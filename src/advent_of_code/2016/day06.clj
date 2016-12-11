(ns advent-of-code.2016.day06
  "Fourth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(def puzzle
  "This is the input from the code to the Easter Bunny's directory. The result
  of this will be a sequence of room maps."
  (-> (slurp "resources/2016/input/day06.txt")
      (cs/trim)
      (cs/split #"\n")))

(defn yank
  "Function to find a character in each position of the strings in the input
  sequence of strings (of the same length), and then build a string from those
  most frequent characters, and the function is how to sort them so that the
  `first` character by that soert, is the one we want."
  [sigs f]
  (let [cnt (count (first sigs))
        bst (fn [p] (->> (map #(nth % p) sigs)
                         (frequencies)
                         (sort-by second f)))]
    (->> (map bst (range cnt))
         (map first)
         (map first)
         (apply str))))

(defn one
  "Function to find the most used character in each position of the strings in
  the input sequence of strings (of the same length), and then build a string
  from those most frequent characters."
  [sigs]
  (yank sigs >))

(defn two
  "Function to find the least used character in each position of the strings in
  the input sequence of strings (of the same length), and then build a string
  from those most frequent characters."
  [sigs]
  (yank sigs <))

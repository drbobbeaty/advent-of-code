(ns advent-of-code.2016.day03
  "Third day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(defn ->vec
  "Function to take a string of the form: '2 3 4' and convert it to a vector
  of integers: [2 3 4]. If we are given a sequences of strings, do this for
  every string in the sequence."
  [arg]
  (cond
    (string? arg) (-> (cs/trim arg)
                      (cs/split #" ")
                      (->> (remove #(= "" %))
                           (map #(Integer/parseInt %)))
                      (vec))
    (coll? arg)   (map ->vec arg)
    :else         arg))

(def puzzle
  "This is the input from the code to the Easter Bunny's artwork. The result
  of this will be a sequence of strings of directions."
  (-> (slurp "resources/2016/input/day03.txt")
      (cs/trim)
      (cs/split #"\n")
      (->vec)))

(def columns
  "This is the 'transposed' data for part 2 - where the rows of three are really
  column definitions of the triangles, and so we reformat the data to make these
  new tirangle definitions."
  (apply concat (for [[a b c] (partition-all 3 puzzle)]
                  (map vector a b c))))

(defn possible?
  "Predicate function to see if the vector of three sides can make a triangle."
  [v]
  (let [sv (sort v)]
    (< (last sv) (apply + (take 2 sv)))))

(defn one
  "Function to look through all the vectors in the input sequence and count how
  many of them are valid triangles."
  [s]
  (count (filter possible? s)))

(ns advent-of-code.2016.day20
  "Twentieth day's solutions for the Advent of Code 2016"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(defn fix
  "Function to parse the string 'x-y' to be a sequence of two longs that are
  a lot easier to deal with than to deal with the strings."
  [s]
  (->> (cs/split s #"-")
    (map #(Long/parseLong (cs/trim %)))))

(def puzzle
  "This is the source list of excluded IP addresses."
  (->> "resources/2016/input/day20.txt"
       (io/reader)
       (line-seq)
       (map fix)))

(defn one
  "Function to look for the _lowest_ IP address that isn't blocked. This is
  done by starting at 0, and looking for a blocked section, and then jumping
  to the high-end of the block, and continue. This allows us to skip all the
  values that are in each block - and stop at the first miss we find."
  [& [n]]
  (let [top 4294967296
        excl puzzle]
    (loop [i (or n 0)]
      (if (< i top)
        (if-let [hit (first (filter #(<= (first %) i (second %)) excl))]
          (recur (inc (second hit)))
          {:first-allowed i})
        {:topped i}))))

(defn two
  "Function to count all the IP addresses that make it through the puzzle
  and return those. We're going to leverage the `one` function as we'll
  just increment the last passing value, and see what the next one is."
  []
  (loop [i 0
         cnt 0]
    (let [{fa :first-allowed :as ans} (one i)]
      (if fa
        (recur (inc fa) (inc cnt))
        {:passes cnt}))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  [& [n]]
  (let [top 10
        excl [[5 8] [0 2] [4 7]]]
    (loop [i (or n 0)]
      (if (< i top)
        (if-let [hit (first (filter #(<= (first %) i (second %)) excl))]
          (recur (inc (second hit)))
          {:first-allowed i})
        {:topped i}))))

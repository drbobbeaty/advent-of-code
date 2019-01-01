(ns advent-of-code.2016.day09
  "Ninth day's solutions for the Advent of Code 2016"
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the code for the Easter Bunny's keypad display. The
  result of this will be a sequence of commands."
  (-> (slurp "resources/2016/input/day09.txt")
      (cs/trim)))

(defn to-int
  "Function to convert the argument to an integer - simply. Nothing fancy, but
  it certainly could be a lot better than this. :)"
  [s]
  (cond
    (number? s) (int s)
    (string? s) (Integer/parseInt (cs/trim s))
    :else       s))

(defn expand
  "Function to 'decompress' the input string based on the rules in the design
  spec for the compression algo."
  [s]
  (loop [done nil
         src s]
    (let [[pre szs cnts tgt] (drop 1 (re-matches #"(.*?)\((\d+)x(\d+)\)(.*)$" src))]
      (if (and cnts szs)
        (let [sz (to-int szs)
              cnt (to-int cnts)
              ex (apply str (flatten (repeat cnt (take sz tgt))))]
          (recur (str done pre ex) (apply str (drop sz tgt))))
        (str done src)))))

(defn expand2
  "Function to 'decompress' the input string based on the version 2 rules in
  the design spec for the compression algo."
  [s]
  (loop [src s]
    (let [[pre szs cnts tgt] (drop 1 (re-matches #"(.*?)\((\d+)x(\d+)\)(.*)$" src))]
      (if (and cnts szs)
        (let [sz (to-int szs)
              cnt (to-int cnts)
              ex (apply str (flatten (repeat cnt (take sz tgt))))]
          (recur (str pre ex (apply str (drop sz tgt)))))
        src))))

(defn ex-len
  "Function to *just* calculate the length of the decompressed string - using
  the version 2 of the algorithm. This is necessary because the puzzle is too
  big to hold in memory, and we need to walk our way through the process."
  [s]
  (loop [src s
         tot 0]
    (let [[pre szs cnts tgt] (drop 1 (re-matches #"(.*?)\((\d+)x(\d+)\)(.*)$" src))]
      (if (and cnts szs)
        (let [sz (to-int szs)
              cnt (to-int cnts)]
          (recur (apply str (drop sz tgt)) (+ tot (count pre) (* cnt (ex-len (apply str (take sz tgt)))))))
        (+ tot (count src))))))

(defn one
  "Function to take the compressed input, or default to the puzzle string, and
  decompress it and return the size of the decompressed and stripped string."
  [& [src]]
  (let [ex (expand (or src puzzle))]
    {:raw (count ex) :stripped (count (filter #(not= \space %) ex))}))

(defn two
  "Function to take the compressed input, or default to the puzzle string, and
  decompress it - using the version 2 decoding - and return the size of the
  decompressed and stripped string."
  [& [src]]
  (ex-len (or src puzzle)))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["ADVENT" "A(1x5)BC" "(3x3)XYZ" "A(2x2)BCD(2x2)EFG" "(6x1)(1x3)A" "X(8x2)(3x3)ABCY"]]
    (doseq [raw src
            :let [ex (expand raw)]]
      (prn raw ex (count ex)))))

(defn bobo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["(3x3)XYZ" "X(8x2)(3x3)ABCY" "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" "(27x12)(20x12)(13x14)(7x10)(1x12)A"]]
    (doseq [raw src]
      (prn raw (ex-len raw)))))

(ns advent-of-code.2016.day16
  "Sixteenth day's solutions for the Advent of Code 2016"
  (require [clojure.java.io :as io]
           [clojure.string :as cs]))

(defn aug
  "Function to take a string input and augment it, per the rules of the puzzle,
  so that it's 'more' for the filling up of the disk."
  [s]
  (let [flp (fn [c] (if (= \0 c) \1 \0))]
    (apply str (flatten [s \0 (map flp (reverse s))]))))

(defn gen
  "Function to start with the provided seed, and generate a string using `aug`
  that is _at least_ `len` characters long, and return that."
  [s len]
  (loop [src s]
    (let [nxt (aug src)]
      (if (< (count nxt) len)
        (recur nxt)
        nxt))))

(defn chksum
  "Function to compute the 'paired checksum' properly, including the recursion
  on the even-lengthed checksum just to be safe."
  [s]
  (let [mat? (fn [[a b]] (if (= a b) \1 \0))]
    (loop [src s]
      (let [cs (apply str (map mat? (partition 2 src)))]
        (if (even? (count cs))
          (recur cs)
          cs)))))

(defn one
  "Function to compute the checksum of the correct disk-filling data for
  covering our tracks."
  []
  (let [len 272
        disk (.substring (gen "01110110101001000" len) 0 len)]
    (chksum disk)))

(defn two
  "Function to compute the checksum of the correct disk-filling data for
  covering our tracks."
  []
  (let [len 35651584
        disk (.substring (gen "01110110101001000" len) 0 len)]
    (chksum disk)))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [len 20
        disk (.substring (gen "10000" len) 0 len)]
    (chksum disk)))

(ns advent-of-code.2016.day14
  "Fourteenth day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(defn md5
  "Function to compute the MD5 hash of the input string and return it as a
  hex string - all lowercase."
  [s]
  (let [algo (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algo (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn trip
  "Function to test the supplied string for three of the same character in
  a row, and if found, return that character as a string. If not, return
  nil."
  [s]
  (if-let [[_ c] (re-matches #".*?(.)\1\1.*" s)] c))

(defn genkey
  "Function to generate a sequence of keys based on the provided seed, and
  then drop the specified number before returning the next one. This is what
  the first part of the puzzle needs."
  [[seed hc] dc]
  (let [hashes (map (fn [i] [i (last (take (inc hc) (iterate md5 (str seed i))))]) (range))]
    (->> (for [p (partition 1001 1 hashes)
               :let [[i h] (first p)
                     tr (trip h)]
               :when tr
               :let [tgt (apply str (repeat 5 tr))]
               :when (some #(cs/includes? % tgt) (map second (rest p)))]
           [i h])
      (drop dc)
      (first))))

(defn one
  "Function to pull the 65th key based on the key in the puzzle."
  []
  (genkey ["ahsbgdzn" 1] 63))

(defn two
  "Function to pull the 65th key based on the key in the puzzle."
  []
  (genkey ["ahsbgdzn" 2017] 63))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (genkey ["abc" 1] 63))

(defn bobo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (genkey ["abc" 2017] 63))

(ns advent-of-code.2020.day14
  "Fourteenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to parse the input into masks and assignments, so that the engine
  that will run on the data will be able to easily tell them apart."
  [s]
  (let [[msk] (rest (re-matches #"^mask = (.+)$" s))]
    (if (not-empty msk)
      {:mask msk}
      (let [[adr val] (rest (re-matches #"mem\[(\d+)\] = (\d+)$" s))]
        {:addr (parse-int adr) :value (parse-int val)}))))

(def puzzle
  "This is the input of the sea port's computer system."
  (-> (slurp "resources/2020/input/day14.txt")
      (trim)
      (split #"\n")
      (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
       "mem[8] = 11"
       "mem[7] = 101"
       "mem[8] = 0"]
      (->> (map parse))))

(def test2
  "Test data for the second part."
  (-> ["mask = 000000000000000000000000000000X1001X"
       "mem[42] = 100"
       "mask = 00000000000000000000000000000000X0XX"
       "mem[26] = 1"]
      (->> (map parse))))

(defn band
  "Function to perform a bit-wise forcing of the value and the bitmask provided.
  In the mask, an 'X' is nothing, a '1' and '0' are set on the value's bits, and
  the decimal value is returned. It's a bit like AND/OR, but just forcing the
  values, as they appear in the mask."
  [v bm]
  (let [bv (str "000000000000000000000000000000000000" (Long/toString v 2))]
    (-> (for [[i t] (map vector (reverse bv) (reverse bm))]
          (if (= t \X) i t))
        (reverse)
        (->> (apply str))
        (Long/parseLong 2))))

(defn exec
  "Function to take a memory layout - address and values as the key/value pairs,
  and then a sequence on mask/memory commands that need to be executed against
  that memory space, and the result returned to the caller."
  [m pgm]
  (loop [cmds pgm
         mem m
         mask nil]
    (if-let [c (first cmds)]
      (if (contains? c :mask)
        (recur (rest cmds) mem (:mask c))
        (recur (rest cmds) (assoc mem (:addr c) (band (:value c) mask)) mask))
      mem)))

(defn one
  "Function to run through the code, masking and setting values, and then
  summing up all the memory values we have and returning that."
  [& [m]]
  (sum (vals (exec {} puzzle))))

(defn two
  "Function to "
  [& [m]]

  )

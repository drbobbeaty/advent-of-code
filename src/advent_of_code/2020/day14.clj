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

(defn mask-val
  "Function to update the memory with the provided command and mask. This will
  perform a bit-wise forcing of the value and the bitmask provided - as per the
  instructions for part 1. In the mask, an 'X' is nothing, a '1' and '0' are
  set on the value's bits, and the decimal value is returned. It's a bit like
  AND/OR, but just forcing the values, as they appear in the mask. The return
  value is the updated memory map."
  [mem {adr :addr val :value :as cmd} mask]
  (let [bv (str "000000000000000000000000000000000000" (Long/toString val 2))
        nv (-> (for [[i t] (map vector (reverse bv) (reverse mask))]
                 (if (= t \X) i t))
               (reverse)
               (->> (apply str))
               (Long/parseLong 2))]
    (assoc mem adr nv)))

(defn exec
  "Function to take a memory layout - address and values as the key/value pairs,
  and then a sequence on mask/memory commands that need to be executed against
  that memory space, and the result returned to the caller."
  [m pgm updfn]
  (loop [cmds pgm
         mem m
         mask nil]
    (if-let [c (first cmds)]
      (if (contains? c :mask)
        (recur (rest cmds) mem (:mask c))
        (recur (rest cmds) (updfn mem c mask) mask))
      mem)))

(defn one
  "Function to run through the code, masking and setting values, and then
  summing up all the memory values we have and returning that."
  [& [m]]
  (sum (vals (exec {} puzzle mask-val))))

(def rev-bits
  "A sequence of numbers - but in a unique format. These will be the binary
  representation of each number, in order, but with the bits reversed so that
  they can more easily be put into the floating positions in the address space."
  (for [i (range 1024)]
    (-> (str "000000000000000000000000000000000000" (Long/toString i 2))
        (reverse)
        (->> (take 36)))))

(defn mk-addr
  "Function to take the source from part 2 - with 'X' and digits, in the
  reverse order, and create a sequence of decimal numbers that are the address
  values for the Xs being replaced by increasing binary numbers - regardless of
  their location. This is done reversed, to make sure that we start with the
  smallest inserted number, and move up."
  [arg]
  (let [fs (take (int (Math/pow 2 (count (filter #(= \X %) arg)))) rev-bits)]
    (for [f fs]
      (loop [src arg
             fbs f
             ans (transient [])]
        (if-let [c (first src)]
          (if (= \X c)
            (recur (rest src) (rest fbs) (conj! ans (first fbs)))
            (recur (rest src) fbs (conj! ans c)))
          (Long/parseLong (apply str (reverse (persistent! ans))) 2))))))

(defn float-val
  "Function to update the memory with the provided command and mask. This will
  perform a bit-wise forcing of the value and the bitmask provided - as per the
  instructions for part 1. In the mask, an 'X' is nothing, a '1' and '0' are
  set on the value's bits, and the decimal value is returned. It's a bit like
  AND/OR, but just forcing the values, as they appear in the mask. The return
  value is the updated memory map."
  [mem {adr :addr val :value :as cmd} mask]
  (let [ba (str "000000000000000000000000000000000000" (Long/toString adr 2))
        src (for [[i t] (map vector (reverse ba) (reverse mask))]
              (if (= t \0) i t))]
    (apply assoc mem (interleave (mk-addr src) (repeat val)))))

(defn two
  "Function to complete part 2 where we look at the masks as different rules,
  and the addresses are mapped out in the memory space. Just a lot of little
  tests and housekeeping."
  [& [m]]
  (sum (vals (exec {} puzzle float-val))))

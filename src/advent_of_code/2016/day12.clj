(ns advent-of-code.2016.day12
  "Twelveth day's solutions for the Advent of Code 2016"
  (:require [clojure.string :as cs]))

(defn ->int
  "Parses a string into an int, expecting \"Inf\" for infinity. A nil is parsed
  as 0 by default - similar to ruby's `to_i` method."
  [x]
  (cond
    (nil? x) 0
    (string? x) (if (empty? x)
                  0
                  (try
                    (Integer/parseInt (cs/trim x))
                    (catch java.lang.NumberFormatException nfe
                      0)))
    (coll? x) (map ->int x)
    (number? x) (int (if (pos? x) (min x Integer/MAX_VALUE) (max x Integer/MIN_VALUE)))
    :else x))

(defn code-it
  "Function to do the simple parsing of the input from the AoC site. Just
  makes sense to provide some structure to the data so that it's easier to
  work with."
  [s]
  (cond
    (string? s) (cond
                  (.startsWith s "cpy")
                    (let [[src dst] (drop 1 (re-matches #"^cpy (\S+) (\S+)$" s))]
                      {:action :cpy
                       :src (if (re-matches #"[0-9]+" src) (->int src) (keyword src))
                       :register (keyword dst)})
                  (.startsWith s "inc")
                    (let [[r] (drop 1 (re-matches #"^inc (\S+)$" s))]
                      {:action :inc
                       :register (keyword r)})
                  (.startsWith s "dec")
                    (let [[r] (drop 1 (re-matches #"^dec (\S+)$" s))]
                      {:action :dec
                       :register (keyword r)})
                  (.startsWith s "jnz")
                    (let [[t d] (drop 1 (re-matches #"^jnz (\S+) (\S+)$" s))]
                      {:action :jnz
                       :test (if (re-matches #"[0-9]+" t) (->int t) (keyword t))
                       :dest (->int d)}))
    (coll? s)   (map code-it s)
    :else       s))

(def puzzle
  "This is the instructions for the CPU control."
  (-> (slurp "resources/2016/input/day12.txt")
      (cs/trim)
      (cs/split #"\n")))

(defn run
  "Function to run the supplied program in a machine, and then return the
  status of the registers once the program is complete."
  [& [src]]
  (let [inst (code-it (or src puzzle))
        cnt (count inst)
        reg (atom {})]
    (loop [pc 0]
      (when-let [i (nth inst pc nil)]
        (recur
          (case (:action i)
            :cpy (let [sv (:src i)
                       v (if (keyword? sv) (or (sv @reg) 0) sv)]
                   (swap! reg assoc (:register i) v)
                   (inc pc))
            :inc (do
                   (swap! reg update (:register i) #(inc (or % 0)))
                   (inc pc))
            :dec (do
                   (swap! reg update (:register i) #(dec (or % 0)))
                   (inc pc))
            :jnz (let [ti (:test i)
                       tv (if (keyword? ti) (or (ti @reg) 0) ti)
                       os (if-not (zero? tv) (:dest i) 1)]
                  (+ pc os))))))
    @reg))

(defn one
  "Function to take a sequence of instructions as the argument, or use the
  puzzle as the default, and return the state of the world once it's all
  done."
  [& [src]]
  (run src))

(defn two
  "Function to take a sequence of instructions as the argument, or use the
  puzzle as the default, and return the state of the world once it's all
  done."
  []
  (run (conj (seq puzzle) "cpy 1 c")))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["cpy 41 a"
             "inc a"
             "inc a"
             "dec a"
             "jnz a 2"
             "dec a"]]
    (run src)))

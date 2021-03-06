(ns advent-of-code.2016.day23
  "Twenty-third day's solutions for the Advent of Code 2016"
  (require [clojure.java.io :as io]
           [clojure.math.combinatorics :as cmc]
           [clojure.string :as cs]))

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
                       :src (if (re-matches #"[0-9\-]+" src) (->int src) (keyword src))
                       :dest (keyword dst)})
                  (.startsWith s "inc")
                    (let [[r] (drop 1 (re-matches #"^inc (\S+)$" s))]
                      {:action :inc
                       :dest (keyword r)})
                  (.startsWith s "dec")
                    (let [[r] (drop 1 (re-matches #"^dec (\S+)$" s))]
                      {:action :dec
                       :dest (keyword r)})
                  (.startsWith s "jnz")
                    (let [[t d] (drop 1 (re-matches #"^jnz (\S+) (\S+)$" s))]
                      {:action :jnz
                       :src (if (re-matches #"[0-9\-]+" t) (->int t) (keyword t))
                       :dest (if (re-matches #"[0-9\-]+" d) (->int d) (keyword d))})
                  (.startsWith s "tgl")
                    (let [[d] (drop 1 (re-matches #"^tgl (\S+)$" s))]
                      {:action :tgl
                       :dest (if (re-matches #"[0-9\-]+" d) (->int d) (keyword d))}))
    (coll? s)   (map code-it s)
    :else       s))

(def puzzle
  "This is the instructions for the CPU control."
  (-> "resources/2016/input/day23.txt"
    (io/reader)
    (line-seq)))

(defn run
  "Function to run the supplied program in a machine, and then return the
  status of the registers once the program is complete."
  [& [src]]
  (let [inst (atom (vec (code-it (or src puzzle))))
        cnt (count @inst)
        reg (atom {})]
    (loop [pc 0]
      (when-let [i (nth @inst pc nil)]
        (recur
          (case (:action i)
            :cpy (let [sv (:src i)
                       v (if (keyword? sv) (or (sv @reg) 0) sv)]
                   (if (keyword? (:dest i))
                     (swap! reg assoc (:dest i) v))
                   (inc pc))
            :inc (let [di (:dest i)]
                   (if (keyword? di)
                     (swap! reg update di #(inc (or % 0))))
                   (inc pc))
            :dec (let [di (:dest i)]
                   (if (keyword? di)
                     (swap! reg update di #(dec (or % 0))))
                   (inc pc))
            :jnz (let [ti (:src i)
                       tv (if (keyword? ti) (or (ti @reg) 0) ti)
                       di (:dest i)
                       os (if-not (zero? tv) (if (keyword? di) (or (di @reg) 0) di) 1)]
                   (+ pc os))
            :tgl (let [di (:dest i)
                       dv (if (keyword? di) (or (di @reg) 0) di)
                       mi (nth @inst (+ pc dv) nil)
                       ni (case (:action mi)
                            :inc (assoc mi :action :dec)
                            :dec (assoc mi :action :inc)
                            :jnz (assoc mi :action :cpy)
                            :cpy (assoc mi :action :jnz)
                            :tgl (assoc mi :action :inc)
                            nil)]
                   (if (and ni (<= 0 (+ pc dv) cnt))
                     (swap! inst assoc (+ pc dv) ni))
                   (inc pc))))))
    @reg))

(defn one
  "Function to run the instructions with the new `tgl` instruction added to the
  set, and see what we need to send to the safe. We need to start by seeding
  register 'a' with 7 - the number of eggs in the painting."
  [& [src]]
  (run (concat ["cpy 7 a"] (or src puzzle))))

(defn two
  "Function to run the instructions with the new `tgl` instruction added to the
  set, and see what we need to send to the safe. We need to start by seeding
  register 'a' with 7 - the number of eggs in the painting."
  [& [src]]
  (run (concat ["cpy 12 a"] (or src puzzle))))

(defn yoyo
  "Function to run the simple example in the puzzle to see that we have all the
  parts working properly."
  []
  (let [src ["cpy 2 a"
             "tgl a"
             "tgl a"
             "tgl a"
             "cpy 1 a"
             "dec a"
             "dec a"]]
    (run src)))

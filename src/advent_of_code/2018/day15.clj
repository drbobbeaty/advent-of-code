(ns advent-of-code.2018.day15
  "Fourteenth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]
            [clojure.walk :as cw]))

(def puzzle
  "This is the input of the location of the battle royale area."
  (-> (slurp "resources/2018/input/day15.txt")
      (cs/trim)
      (cs/split #"\n")
      (as-> s (map vec s))))

(defn bget
  "Function to get the value of the battlefield at column `x`, and row
  `y`. If nothing is there, a character space is returned - just to make
  the comparisons easier."
  [bf x y]
  (-> (nth bf y [])
      (nth x \space)))

(defn overlay
  "Function to overlay the players on the battlefield so that we can get
  a complete picture of what's where, and how the state of the battle is."
  [bf ap]
  (loop [bfr bf
         ps ap]
    (if-let [{c :char [x y] :pos} (first ps)]
      (recur (->> (vec (assoc (nth bfr y []) x c))
               (assoc (vec bfr) y)
               (vec))
             (rest ps))
      bfr)))

(defn open?
  "Predicate function to take a battlefield, a sequence of players, and
  a location in the battlefield, and returns `true` if the space is open,
  meaning not a wall or a player."
  ([bf x y]
    (#{\. \space} (bget bf x y)))
  ([bf ps x y]
    (open? (overlay bf ps) x y)))

(defn findp
  "Function to extract all the elves and goblins on the battlefield and
  for each, return a map of their location - this is how we will track
  each of them."
  [bf]
  (let [war #{\E \G}]
    (for [[y row] (map vector (range) bf)
          [x c] (map vector (range) row)
          :when (war c)]
      {:char c :pos [x y] :hp 200 :pwr 3})))

(defn sortp
  "Function to sort the players by their location in the battlefield, so
  that we are ready to move each towards opponents, attack, etc."
  [sp]
  (sort-by (comp (juxt second first) :pos) sp))

(defn clearp
  "Function to remove all the elves and goblins from the battfield map so
  that it's ready to be the 'back-drop' for the movement of the players.
  This returns the battlefield map with all the players replaced with the
  empty place marker - '.'"
  [bf]
  (map #(cw/postwalk-replace {\E \. \G \.} %) bf))

(defn pretty
  "Function to make a pretty view of the battlefield for outputting."
  [bf]
  (let [fmt (fn [s] (format "%s" (apply str s)))]
    (cs/join "\n" (map fmt bf))))

(defn blog
  "Function to make a nice log message for the provided battlefield. This
  will make it easy to see what's happening."
  [bf & [msg]]
  (info
    (if msg
      (str msg "\n" (pretty bf))
      (str "battlefield:\n" (pretty bf)))))

(defn one
  ""
  [& [n]]
  nil)

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  nil)

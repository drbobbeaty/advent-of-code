(ns advent-of-code.2020.day11
  "Eleventh day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a series of strings, showing the layout of the waiting room
  in the ferry terminal, and convert this to a map of location [row col] and
  what's at that location - if it's not the floor."
  [coll]
  (into {}
    (for [[r ss] (map vector (range) coll)
          [c s] (map vector (range) ss)
          :when (= \L s)]
      [[r c] s])))

(def puzzle
  "This is the input of the initial seat layout at the ferry terminal."
  (-> (slurp "resources/2020/input/day11.txt")
      (trim)
      (split #"\n")
      (parse)))

(def test1
  "Test data for the first part."
  (-> ["L.LL.LL.LL"
       "LLLLLLL.LL"
       "L.L.L..L.."
       "LLLL.LL.LL"
       "L.LL.LL.LL"
       "L.LLLLL.LL"
       "..L.L....."
       "LLLLLLLLLL"
       "L.LLLLLL.L"
       "L.LLLLL.LL"]
      (parse)))

(defn neighbors
  "Function to return all the neighbors to the provided seat location, and
  what their status is - empty or occupied. This will be used over and over
  to see the status of each seat in the waiting room, so we can act on it."
  [[r c] brd]
  (for [dr [-1 0 1] dc [-1 0 1]
        :when (not (= 0 dr dc))
        :let [x (get brd [(+ r dr) (+ c dc)] \.)]
        :when (not= x \.)]
    x))

(defn evolve
  "Function to take a board, and look at all the seat locations, and update
  each based on the current status of the neighboring seats, and the rules we
  have been given. This returns a new seating map, and that can then be
  used for a lot of things."
  [brd efn lvl]
  (into {}
    (for [[k v] brd
          :let [ocnt (count (filter #(= \# %) (efn k brd)))]]
      (cond
        (and (= v \L) (= 0 ocnt)) [k \#]
        (and (= v \#) (<= lvl ocnt)) [k \L]
        :else [k v]))))

(defn one
  "Function to start with a seating arrangement, and evolve it until it's
  unchanging over time, and then return the number of occupied seats. While
  we're at it, return the number of evolutions it took to achieve this."
  [& [m]]
  (loop [b (or m puzzle)
         cnt 0]
    (let [nb (evolve b neighbors 4)]
      (if (not= b nb)
        (recur nb (inc cnt))
        {:cycles cnt :occupied (count (filter #(= \# %) (vals nb)))}))))

(defn sights
  "Function to return all the visible neighbors to the provided seat location,
  and what their status is - empty or occupied. This will be used over and over
  to see the status of each seat in the waiting room, so we can act on it."
  [[r c] brd]
  (let [rmax (apply max (map first (keys brd)))
        cmax (apply max (map last (keys brd)))
        see (fn [dr dc]
              (loop [y (+ r dr)
                     x (+ c dc)]
                (if (and (<= 0 y rmax) (<= 0 x cmax))
                  (let [pc (get brd [y x] \.)
                       ]
                    (if (= \. pc)
                      (recur (+ y dr) (+ x dc))
                      pc))
                  \.)))]
    (for [dr [-1 0 1] dc [-1 0 1]
          :when (not (= 0 dr dc))
          :let [x (see dr dc)]
          :when (not= x \.)]
      x)))

(defn two
  "Function to start with the seating arrangement, and then use a different
  method for 'finding' the neighbors, and this one is far more computationally
  expensive, but it gets the job done."
  [& [m]]
  (loop [b (or m puzzle)
         cnt 0]
    (let [nb (evolve b sights 5)]
      (infof "cycle: %d" cnt)
      (if (not= b nb)
        (recur nb (inc cnt))
        {:cycles cnt :occupied (count (filter #(= \# %) (vals nb)))}))))

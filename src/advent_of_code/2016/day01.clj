(ns advent-of-code.2016.day01
  "First day's solutions for the Advent of Code 2016"
  (:require [clojure.string :as cs]))

(def puzzle
  "This is the input from the path to the Easter Bunny HQ. The result of this
  will be a sequence of tuples of the form: ['R' 3] - a turn and a distance."
  (-> (slurp "resources/2016/input/day01.txt")
      (cs/trim)
      (cs/split #"\, ")
      (->> (map (fn [s] [(subs s 0 1) (Integer/parseInt (subs s 1))])))))

(defn turn
  "Function to take a heading and a direction to turn and capculate a new heading."
  [h t]
  (cond
    (= :n h) (if (= "R" t) :e :w)
    (= :e h) (if (= "R" t) :s :n)
    (= :s h) (if (= "R" t) :w :e)
    (= :w h) (if (= "R" t) :n :s)))

(defn move
  "Function to take a location on the plane and a movement direction (N, E, S, W)
  and return the new location resulting in one step."
  [[x y] h]
  (case h
    :n [x (inc y)]
    :e [(inc x) y]
    :s [x (dec y)]
    :w [(dec x) y]
    [x y]))

(defn steps
  "Function to make a sequence of movements: N, S, E, W - based on the starting
  heading and a sequence of puzzle steps. This will then be a step-by-step map
  of how to move to find the Easter Bunny HQ."
  [hing ps]
  (loop [h hing
         s ps
         res []]
    (let [[[[td dd]] rst] (split-at 1 s)
          nh (turn h td)]
      (if-not (empty? rst)
        (recur nh rst (concat res (repeat dd nh)))
        (concat res (repeat dd nh))))))

(defn one
  "Function to take a sequence of steps and move on the cartesian plane according
  to those directions, and come up with the distance the terminal point is as the
  person walks. Meaning, sum the absolute values of the x and y coordinates at
  the end of the walk, and return that."
  [ps]
  (apply + (vals (frequencies (steps :n ps)))))

(defn two
  "Function to track the 'walk' based on the provided step pattern from the
  origin, facing north, and see how far away we are when we cross the path
  we have already made. This just creates a set of all the places we have
  visited, and then compares the new step to the set and stops on a hit."
  [ps]
  (loop [l [0 0]
         ws (steps :n ps)
         pa #{[0 0]}]
    (let [[[st] rst] (split-at 1 ws)
          nl (move l st)]
      (if (pa nl)
        (apply + (map #(Math/abs %) nl))
        (if-not (empty? rst)
          (recur nl rst (conj pa nl)))))))

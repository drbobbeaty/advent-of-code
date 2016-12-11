(ns advent-of-code.2016.day02
  "Second day's solutions for the Advent of Code 2016"
  (require [clojure.string :as cs]))

(def puzzle
  "This is the input from the code to the Eazter Bunny's bathrom'. The result
  of this will be a sequence of strings of directions."
  (-> (slurp "resources/2016/input/day02.txt")
      (cs/trim)
      (cs/split #"\n")))

(defn one
  "Function to implement the structure of the keypad and the movement around it.
  The button `b` and movement `m` allow us to have a board like anything we want,
  and then return the button you'd be on after moving from `b` via `m`."
  [b m]
  (case b
    1 (case m \R 2 \D 4 1)
    2 (case m \R 3 \D 5 \L 1 2)
    3 (case m \D 6 \L 2 3)
    4 (case m \U 1 \R 5 \D 7 4)
    5 (case m \U 2 \R 6 \D 8 \L 4 5)
    6 (case m \U 3 \D 9 \L 5 6)
    7 (case m \U 4 \R 8 7)
    8 (case m \U 5 \R 9 \L 7 8)
    9 (case m \U 6 \L 8 9)))

(defn two
  "Function to implement the structure of the keypad and the movement around it.
  The button `b` and movement `m` allow us to have a board like anything we want,
  and then return the button you'd be on after moving from `b` via `m`."
  [b m]
  (case b
    1  (case m \D 3 1)
    2  (case m \R 3 \D 6 2)
    3  (case m \U 1 \R 4 \D 7 \L 2 3)
    4  (case m \D 8 \L 3 4)
    5  (case m \R 6 5)
    6  (case m \U 2 \R 7 \D :A \L 5 6)
    7  (case m \U 3 \R 8 \D :B \L 6 7)
    8  (case m \U 4 \R 9 \D :C \L 7 8)
    9  (case m \L 8 9)
    :A (case m \U 6 \R :B :A)
    :B (case m \U 7 \R :C \D :D \L :A :B)
    :C (case m \U 8 \L :B :C)
    :D (case m \U :B :D)))

(defn next-code
  "Function to take a starting button `btn`, and a series of movements `stps`,
  and the _movement_ function (that defines the board), and return the button
  you end up on after all those steps are done."
  [btn stps fn]
  (loop [b btn
         s stps]
    (let [[[c] rst] (split-at 1 s)
          nb (fn b c)]
      (if-not (empty? rst)
        (recur nb rst)
        nb))))

(defn code
  "Function to take the puzzle directions and use each to find the bathroom
  code based on the keypad in the `one` function."
  [stps fn]
  (loop [bc []
         s stps]
    (let [[[ms] rst] (split-at 1 s)
          nb (next-code (or (last bc) 5) ms fn)]
      (if-not (empty? rst)
        (recur (conj bc nb) rst)
        (conj bc nb)))))

(defn one-done
  "Function to find out the bathroom code for the first part."
  []
  (code puzzle one))

(defn two-done
  "Function to find out the bathroom code for the second part."
  []
  (code puzzle two))

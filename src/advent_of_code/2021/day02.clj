(ns advent-of-code.2021.day02
  "Second day's solutions for the Advent of Code 2021"
  (:require [advent-of-code.util :refer [parse-int trim split]]))

(defn parse
  "Function to parse the commands into tuples of [dx dy] movements for the
  sub. This will then make it much easier to simply accumulate the changes
  for each command."
  [s]
  (let [[dir cnt] (split s #" ")
        stp (parse-int cnt)]
    (case dir
      "forward" [stp 0]
      "down"    [0 stp]
      "up"      [0 (* -1 stp)])))

(def puzzle
  "This is the input of the planned course for the ship."
  (-> (slurp "resources/2021/input/day02.txt")
    (trim)
    (split #"\n")
    (->> (map parse))))

(def test1
  "Test data for the first part."
  (-> ["forward 5"
       "down 5"
       "forward 8"
       "up 3"
       "down 8"
       "forward 2"]
    (->> (map parse))))

(defn one
  "Function to find the final horizontal position and the final depth
  of the sub after the programmed path, and then multiply them together
  for the answer."
  [& [coll]]
  (let [x (apply + (map first puzzle))
        y (apply + (map last puzzle))]
    {:pos x :depth y :prod (* x y)}))

(defn two
  "Function to find the final horizontal position and the final depth
  of the sub - but using the 'up' and 'down' as changes to the 'aim', as
  opposed to changes in the depth. At the same time, the 'fwd' will also
  update the depth, based on the aim. So using these changes, find out
  where we end up, and the product of the position and depth."
  [& [coll]]
  (loop [plan puzzle
         x    0
         y    0
         aim  0]
    (if-let [[dx daim] (first plan)]
      (recur (rest plan) (+ x dx) (+ y (* aim dx)) (+ aim daim))
      {:pos x :depth y :aim aim :prod (* x y)})))

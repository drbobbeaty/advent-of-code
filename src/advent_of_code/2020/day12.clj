(ns advent-of-code.2020.day12
  "Twelfth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the navigation computer directions."
  (-> (slurp "resources/2020/input/day12.txt")
      (trim)
      (split #"\n")))

(def test1
  "Test data for the first part."
  ["F10" "N3" "F7" "R90" "F11"])

(defn one
  "Function to take a sequence of guidance directions, and to move according to
  them, and return the final position, heading, and manhattan distance moved
  from the starting point."
  [& [coll]]
  (loop [src (or coll puzzle)
         x 0
         y 0
         h 0]
    (if-let [stp (first src)]
      (let [cmd (first stp)
            amt (parse-int (subs stp 1))]
        (case cmd
          \N (recur (rest src) x (+ y amt) h)
          \S (recur (rest src) x (- y amt) h)
          \E (recur (rest src) (+ x amt) y h)
          \W (recur (rest src) (- x amt) y h)
          \L (recur (rest src) x y (mod (+ h amt) 360))
          \R (recur (rest src) x y (mod (- h amt) 360))
          \F (case h
               0   (recur (rest src) (+ x amt) y h)
               90  (recur (rest src) x (+ y amt) h)
               180 (recur (rest src) (- x amt) y h)
               270 (recur (rest src) x (- y amt) h)
               (errorf "we have a non-axial heading: %d" h))
          (errorf "got unknown command: '%s'" stp)))
      {:x x :y y :heading h :dist (+ (Math/abs x) (Math/abs y))})))

(defn two
  "Function to use the different meaning of the directions, with the waypoint,
  and to repeat the navigation, and come up with where we should be, with the
  waypoint, and the manhattan distance traveled."
  [& [coll]]
  (let [turn (fn [ccw amt dx dy]
               (case amt
                 0   [dx dy]
                 90  (if ccw [(* -1 dy) dx] [dy (* -1 dx)])
                 180 [(* -1 dx) (* -1 dy)]
                 270 (if ccw [dy (* -1 dx)] [(* -1 dy) dx])))]
    (loop [src (or coll puzzle)
           x 0
           y 0
           dx 10
           dy 1]
      (if-let [stp (first src)]
        (let [cmd (first stp)
              amt (parse-int (subs stp 1))]
          (case cmd
            \N (recur (rest src) x y dx (+ dy amt))
            \S (recur (rest src) x y dx (- dy amt))
            \E (recur (rest src) x y (+ dx amt) dy)
            \W (recur (rest src) x y (- dx amt) dy)
            (\L \R) (let [[ndx ndy] (turn (= cmd \L) amt dx dy)]
                      (recur (rest src) x y ndx ndy))
            \F (recur (rest src) (+ x (* amt dx)) (+ y (* amt dy)) dx dy)
            (errorf "got unknown command: '%s'" stp)))
        {:x x :y y :dx dx :dy dy :dist (+ (Math/abs x) (Math/abs y))}))))

(ns advent-of-code.2019.day03
  "Third day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(def puzzle
  "This is the input of the paths of the wires for the panel."
  (-> (slurp "resources/2019/input/day03.txt")
      (cs/trim)
      (cs/split #"\n")
      (->> (map #(cs/split % #",")))))

(def trial1
  "The answer to this trial for 'one' is 6 - for 'two' is 30."
  [["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"]])

(def trial2
  "The answer to this trial for 'one' is 159 - for 'two' is 610."
  [["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
   ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])

(def trial3
  "The answer to this trial for 'one' is 135 - for 'two' is 410."
  [["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
   ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"]])

(defn expand
  "Function to expand a path component into a series of delta moves to be
  taken from a point, and then continued on, as needed to the next path step."
  [arg]
  (let [cnt (parse-int (subs arg 1))]
    (case (first arg)
      \U (repeat cnt [0 1])
      \R (repeat cnt [1 0])
      \D (repeat cnt [0 -1])
      \L (repeat cnt [-1 0]))))

(defn path
  "Function to take a sequence of path statements, as specified in the puzzle,
  and convert that into a series of locations the path covers - in the order
  in which it's covering those points. There may be duplicates, if the path
  crosses itself, but those, too, will be in order they are come upon."
  [s]
  (let [loc (atom [0 0])]
    (for [stp s
          [dx dy] (expand stp)
          :let [nx (+ (first @loc) dx)
                ny (+ (second @loc) dy)]]
      (reset! loc [nx ny]))))

(defn mdist
  "Function to calculate the Manhattan distance between the two points, and
  assuming the second is the origin, unless specified."
  [[x y] & [org]]
  (let [dx (- x (or (first org) 0))
        dy (- y (or (second org) 0))]
    (+ (Math/abs dx) (Math/abs dy))))

(defn one
  "Function to look at the two paths, and find all the points where they cross,
  and then find the closest of those to the origin (starting point), based on
  the Manhattan Distance of the coordinates."
  [& [arg]]
  (let [fp (set (distinct (path (first (or arg puzzle)))))]
    (->> (for [sps (distinct (path (second (or arg puzzle))))
               :when (fp sps)]
           sps)
         (sort-by #(mdist %))
         (first)
         (mdist))))

(defn two
  "Function to look at the combined path distance to the intersections, and
  minimize on that metric, as opposed to the distance from the origin. This
  will benefit from the paths being created as sequences, and that's good
  news."
  [& [arg]]
  (let [fp (path (first (or arg puzzle)))
        fps (set fp)
        sp (path (second (or arg puzzle)))
        hits (for [sps sp :when (fps sps)] sps)]
    (->> (for [h hits
               :let [fpl (inc (.indexOf fp h))
                     spl (inc (.indexOf sp h))]]
           [h fpl spl (+ fpl spl)])
         (sort-by last)
         (first)
         (last))))

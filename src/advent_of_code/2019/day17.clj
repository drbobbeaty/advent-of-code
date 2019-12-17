(ns advent-of-code.2019.day17
  "Seventeenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-long sum is-ascii?]]
            [advent-of-code.2019.day05 :refer [run]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the Intcode program - ASCII"
  (-> (slurp "resources/2019/input/day17.txt")
      (cs/trim)
      (cs/split #",")
      (->> (map parse-long))))

(defn image
  "Function to extract the image data from the data stream coming from the
  camera. This puts the data into a format we can use, and render nicely."
  [data]
  (loop [src data
         row 0
         col 0
         tiles (transient {})]
    (if-let [px (first src)]
      (cond
        (= 10 px)
          (recur (rest src) (inc row) 0 tiles)
        (is-ascii? px)
          (recur (rest src) row (inc col) (assoc! tiles [col row] (char px)))
        :else
          (do
            (infof "invalid character value in Intcode output: %d" px)
            (recur (rest src) row (inc col) tiles)))
      (persistent! tiles))))

(defn render
  "Function to render the output of the camera, as defined by the output of
  the Intcode program - ASCII."
  [tiles]
  (let [bts (keys tiles)
        rows (if (empty? bts) 0 (inc (apply max (map second bts))))]
    (for [r (range rows)
          :let [rd (for [[k v] tiles :when (= r (second k))] [k v])]]
        (->> (sort-by (comp first first) rd)
             (map second)
             (apply str)))))

(defn one
  "Function to run the Intcode program - ASCII, and find all the places that
  the scaffolding crosses back on itself, and for each, compute the index, as
  defined by the puzzle. Sum these all up, and you have part 1."
  [& [mem]]
  (let [cpu (run (if (map? mem) mem {:memory (or mem puzzle)}))
        img (image (:output cpu))]
    (->> (for [[[x y] c] img
               :when (= \# c)
               :let [n (img [x (inc y)])
                     e (img [(inc x) y])
                     s (img [x (dec y)])
                     w (img [(dec x) y])]
               :when (= \# n e w s)]
           [[x y] (* x y)])
         (map second)
         (sum))))

(def path
  "The scaffolding for this ship looks like this:

       ..........................#########................
       ..........................#.......#................
       ..........................#.......#................
       ..........................#.......#................
       ......................#########...#................
       ......................#...#...#...#................
       ......................#...#...#...#................
       ......................#...#...#...#................
       ......................#.###########................
       ......................#.#.#...#....................
       ................#####.#.#.#####....................
       ................#...#.#.#..........................
       ................#...#.#########....................
       ................#...#...#.....#....................
       ................#...#...#.....#....................
       ................#...#...#.....#....................
       ................#########.....#....................
       ....................#.........#....................
       ....................#.........#...#######..........
       ....................#.........#...#.....#..........
       #########.###########.........#...#.....#..........
       #.......#.#...................#...#.....#..........
       #.......#.#...................###########..........
       #.......#.#.......................#................
       #.......#.#.......................#...#########....
       #.......#.#.......................#...........#....
       #.......#.#.......................#########...#....
       #.......#.#...............................#...#....
       ###########.............................###########
       ........#...............................#.#...#...#
       ........###########.....................#.#...#...#
       ..................#.....................#.#...#...#
       ..................#.....................#######...#
       ..................#.......................#.......#
       ..................#.......................#.......#
       ..................#.......................#.......#
       ..................#.......................#########
       ..................#................................
       ..................#................................
       ..................#................................
       ..................########^........................

  So the path from for the robot looks to be:

      L,8,R,10,L,10, R,10,L,8,L,8,L,10, L,8,R,10,L,10, L,4,L,6,L,8,L,8,
      R,10,L,8,L,8,L,10, L,4,L,6,L,8,L,8, L,8,R,10,L,10, L,4,L,6,L,8,L,8,
      R,10,L,8,L,8,L,10, L,4,L,6,L,8,L,8

  So the input is as follows:"
  ["A,B,A,C,B,C,A,C,B,C\n"
   "L,8,R,10,L,10\n"
   "R,10,L,8,L,8,L,10\n"
   "L,4,L,6,L,8,L,8\n"])

(defn two
  "Function to use the patterned input from the map, above, and extract the
  non-char value from the final output as the amount of dust it cleaned up
  while on the scaffolding looking for the other robots to turn them on."
  [& [mem]]
  (let [ini (if (map? mem) mem {:memory (or mem (assoc (vec puzzle) 0 2))})
        inp (map int (apply str (conj path "n\n")))
        cpu (run (assoc ini :input inp))
       ]
    (infof "cpu: %s" (pr-str (dissoc cpu :memory :output)))
    (render (image (:output cpu)))))

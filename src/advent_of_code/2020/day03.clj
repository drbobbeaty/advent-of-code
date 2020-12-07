(ns advent-of-code.2020.day03
  "Third day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int xor trim split]]))

(def puzzle
  "This is the input the pattern of the trees on the way to the airport."
  (-> (slurp "resources/2020/input/day03.txt")
      (trim)
      (split #"\n")))

(def test1
  "Test data for the first part."
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(defn hits
  "Function to take a map of trees, and the trajectory increments, right, and
  down, and count how many trees you'd hit on the way to the airport."
  [trees rt dn]
  (let [h (count trees)
        w (count (first trees))]
    (loop [y 0
           x 0
           hits 0]
      (if (< y h)
        (let [row (nth trees y)
              pos (mod x w)]
          (recur (+ y dn) (+ x rt) (if (= \# (.charAt row pos)) (inc hits) hits)))
        hits))))

(defn one
  "Function to find how many trees I'd hit if I took the 'right 3, down 1'
  path through the forest to the airport."
  [& [coll]]
  (hits (or coll puzzle) 3 1))

(defn two
  "Function to look at several different trajectories on the way to the
  airport, and to calculate the hits on each, and then multiply them all
  together."
  [& [coll]]
  (let [trials [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (->> (for [[r d] trials] {:rt r :dn d :cnt (hits (or coll puzzle) r d)})
         (map :cnt)
         (apply *))))

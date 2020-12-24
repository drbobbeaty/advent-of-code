(ns advent-of-code.2020.day24
  "Twenty-fourth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the input of the moves on the hex tiles will make the initial tiles
  black."
  (-> (slurp "resources/2020/input/day24.txt")
      (trim)
      (split #"\n")))

(def test1
  "Test data for the first part."
  ["sesenwnenenewseeswwswswwnenewsewsw"
   "neeenesenwnwwswnenewnwwsewnenwseswesw"
   "seswneswswsenwwnwse"
   "nwnwneseeswswnenewneswwnewseswneseene"
   "swweswneswnenwsewnwneneseenw"
   "eesenwseswswnenwswnwnwsewwnwsene"
   "sewnenenenesenwsewnenwwwse"
   "wenwwweseeeweswwwnwwe"
   "wsweesenenewnwwnwsenewsenwwsesesenwne"
   "neeswseenwwswnwswswnw"
   "nenwswwsewswnenenewsenwsenwnesesenew"
   "enewnwewneswsewnwswenweswnenwsenwsw"
   "sweneswneswneneenwnewenewwneswswnese"
   "swwesenesewenwneswnwwneseswwne"
   "enesenwswwswneneswsenwnewswseenwsese"
   "wnwnesenesenenwwnenwsewesewsesesew"
   "nenewswnwewswnenesenwnesewesw"
   "eneswnwswnwsenenwnwnwwseeswneewsenese"
   "neswnwewnwnwseenwseesewsenwsweewe"
   "wseweeenwnesenwwwswnew"])

(defn move
  "Function to move one step, or a series of steps on the hex grid, starting
  at the supplied point, and moving through the grid as instructe to by the
  steps."
  [pos step]
  (if (string? step)
    (loop [[x y] pos
           stps step]
      (if (< 0 (count stps))
        (cond
          (.startsWith stps "e")  (recur [(inc x) y] (subs stps 1))
          (.startsWith stps "se") (recur [(inc x) (dec y)] (subs stps 2))
          (.startsWith stps "sw") (recur [x (dec y)] (subs stps 2))
          (.startsWith stps "w")  (recur [(dec x) y] (subs stps 1))
          (.startsWith stps "nw") (recur [(dec x) (inc y)] (subs stps 2))
          (.startsWith stps "ne") (recur [x (inc y)] (subs stps 2)))
        [x y]))
    pos))

(defn mk-brd
  "Function to take a sequence of moves, and at the end of each set of moves,
  the tile will be black. This is the processing that starts the other parts."
  [coll]
  (loop [mvs (or coll puzzle)
         blks (transient {})]
    (if-let [fm (first mvs)]
      (let [sp (move [0 0] fm)]
        (if (contains? blks sp)
          (recur (rest mvs) (dissoc! blks sp))
          (recur (rest mvs) (assoc! blks sp 1))))
      (persistent! blks))))

(defn one
  "Function to count the number of black tiles after all the paths in the
  puzzle input."
  [& [coll]]
  (let [brd (mk-brd (or coll puzzle))]
    (count brd)))

(defn hood
  "Function to look at the hex board around the point [x y], and return the
  number of black tiles. That's what will be used to determine the evolution
  of the floor - day to day."
  [[x y] brd]
  (sum (for [[dx dy] [[1 0] [0 1] [-1 1] [-1 0] [0 -1] [1 -1]]
             :let [np [(+ x dx) (+ y dy)]]]
         (if (contains? brd np) 1 0))))

(defn aday
  "Function to evolve the hex floor tiles for a single day."
  [brd]
  (let [xs (map first (keys brd))
        xmin (- (apply min xs) 2)
        xmax (+ (apply max xs) 3)
        ys (map second (keys brd))
        ymin (- (apply min ys) 2)
        ymax (+ (apply max ys) 3)
        blk? (fn [x y]
               (if (contains? brd [x y])
                 (<= 1 (hood [x y] brd) 2)
                 (= 2 (hood [x y] brd))))]
    (into {}
      (for [x (range xmin xmax) y (range ymin ymax)
            :when (blk? x y)]
        [[x y] 1]))))

(defn two
  "Function to do 100 days of iterations on the floor tiles and then print
  out how many are black."
  [& [coll]]
  (loop [brd (mk-brd (or coll puzzle))
         cnt 0]
    (if (< cnt 100)
      (recur (aday brd) (inc cnt))
      (count brd))))

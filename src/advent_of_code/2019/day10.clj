(ns advent-of-code.2019.day10
  "Tenth day's solutions for the Advent of Code 2019"
  (:require [advent-of-code.util :refer [parse-int]]
            [clojure.string :as cs]))

(defn coord
  "Function to take an Elf map of the asteroids, and then convert that to a
  sequence of coordinates for all the asteroids."
  [coll]
  (for [[r data] (map vector (range) coll)
        [c bit] (map vector (range) data)
        :when (= \# bit)]
    [c r]))

(def puzzle
  "This is the input of the paths of the wires for the panel."
  (-> (slurp "resources/2019/input/day10.txt")
      (cs/trim)
      (cs/split #"\n")
      (coord)))

(def trial1
  "Test data for part 1 - best is at [3 4] w/ 8"
  (coord [".#..#"
          "....."
          "#####"
          "....#"
          "...##"]))

(def trial2
  "Test data for part 1 - best is at [5 8] w/ 33"
  (coord ["......#.#."
          "#..#.#...."
          "..#######."
          ".#.#.###.."
          ".#..#....."
          "..#....#.#"
          "#..#....#."
          ".##.#..###"
          "##...#..#."
          ".#....####"]))

(def trial3
  "Test data for part 1 - best is at [1 2] w/ 35"
  (coord ["#.#...#.#."
          ".###....#."
          ".#....#..."
          "##.#.#.#.#"
          "....#.#.#."
          ".##..###.#"
          "..#...##.."
          "..##....##"
          "......#..."
          ".####.###."]))

(def trial4
  "Test data for part 1 - best is at [6 3] w/ 41"
  (coord [".#..#..###"
          "####.###.#"
          "....###.#."
          "..###.##.#"
          "##.##.#.#."
          "....###..#"
          "..#.#..#.#"
          "#..#.#.###"
          ".##...##.#"
          ".....#.#.."]))

(def trial5
  "Test data for part 1 - best is at [11 13] w/ 210"
  (coord [".#..##.###...#######"
          "##.############..##."
          ".#.######.########.#"
          ".###.#######.####.#."
          "#####.##.#.##.###.##"
          "..#####..#.#########"
          "####################"
          "#.####....###.#.#.##"
          "##.#################"
          "#####.##.###..####.."
          "..######..##.#######"
          "####.##.####...##..#"
          ".#####..#.######.###"
          "##...#.##########..."
          "#.##########.#######"
          ".####.#.###.###.#.##"
          "....##.##.###..#####"
          ".#.#.###########.###"
          "#.#.#.#####.####.###"
          "###.##.####.##.#..##"]))

(defn one
  "Function to take a sequence of locations for asteroids and return the one
  location that has a direct line of sight to the most other asteroids in the
  field."
  [& [coll]]
  (let [s (or coll puzzle)
        rad (/ 180 Math/PI)
        aim (fn [[ox oy] [tx ty]]
              (let [a (- 90 (* (Math/atan2 (- oy ty) (- tx ox)) rad))]
                (if (neg? a) (+ a 360) a)))]
    (->> (for [p s
               :let [cnt (->> (for [t s :when (not= p t)] (aim p t))
                              (distinct)
                              (count))]]
           [p cnt])
         (sort-by last)
         (last))))

(defn two
  "Functon to order all the asteroids in firing sequence, where the first
  one hit on any angle is the closest, and then once it's gone, another
  rotation later, the next will be hit."
  [& [coll]]
  (let [s (or coll puzzle)
        rad (/ 180 Math/PI)
        mp (first (one s))
        [mx my] mp
        fld (remove #(= mp %) s)
        aim (fn [[ox oy] [tx ty]]
              (let [a (- 90 (* (Math/atan2 (- oy ty) (- tx ox)) rad))]
                (if (neg? a) (+ a 360) a)))
        dis (fn [[tx ty]] (Math/sqrt (+ (* (- tx mx) (- tx mx)) (* (- ty my) (- ty my)))))
        tgts (transient [])]
    ;; add 360 to each point in a line as they are further from the 'mp'
    (doseq [grp (->> (for [p (remove #(= mp %) s)] [p (aim mp p)])
                     (sort-by last)
                     (partition-by last))]
      (if (= 1 (count grp))
        (conj! tgts (first grp))
        (doseq [[i [p a]] (map vector (range) (sort-by #(dis (first %)) grp))]
          (conj! tgts [p (+ (* i 360.0) a)]))))
    ;; all they want is the 200th point in the firing sequence
    (nth (sort-by second (persistent! tgts)) (dec 200))))

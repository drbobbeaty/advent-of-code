(ns advent-of-code.2020.day17
  "Seventeenth day's solutions for the Advent of Code 2020"
  (:require [advent-of-code.util :refer [parse-int trim split sum]]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(defn parse
  "Function to take a series of strings, showing the layout of the 3-D space
  around the origin for the start of the boot-process."
  [coll]
  (into {}
    (for [[r ss] (map vector (range) coll)
          [c s] (map vector (range) ss)
          :when (= \# s)]
      [[c r 0] s])))

(def puzzle
  "This is the input of "
  (-> (slurp "resources/2020/input/day17.txt")
      (trim)
      (split #"\n")
      (parse)))

(def test1
  "Test data for the first part."
  (-> [".#."
       "..#"
       "###"]
      (parse)))

(def test2
  "Test data for the second part."
  (into {}
    (for [[k v] test1] [(conj k 0) v])))

(defn evolve
  "Function to take the 3D space of active cubes, and run them through one
  cycle and return the new map f the active nodes int he 3D space."
  [brd]
  (let [acts (fn [brd [x y z]]
               (count
                 (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
                       :when (and (not (= 0 dx dy dz))
                                  (= \# (get brd [(+ x dx) (+ y dy) (+ z dz)])))]
                   1)))
        cords (keys brd)
        xs (map first cords)
        [xl xh] [(apply min xs) (apply max xs)]
        ys (map second cords)
        [yl yh] [(apply min ys) (apply max ys)]
        zs (map last cords)
        [zl zh] [(apply min zs) (apply max zs)]]
    (into {}
      (for [x (range (dec xl) (+ 2 xh))
            y (range (dec yl) (+ 2 yh))
            z (range (dec zl) (+ 2 zh))
            :let [pt [x y z]
                  an (acts brd pt)]]
        (if (= \# (get brd pt \.))
          (if (<= 2 an 3) [pt \#])
          (if (= an 3) [pt \#]))))))

(defn one
  "Function to run the puzzle through 6 cycles and then print out the number
  of active nodes in the 3D matrix."
  [& [m]]
  (loop [brd (or m puzzle)
         cnt 0]
    (if (< cnt 6)
      (recur (evolve brd) (inc cnt))
      (count brd))))

(defn evolve2
  "Function to take the 4D space of active cubes, and run them through one
  cycle and return the new map f the active nodes int he 4D space."
  [brd]
  (let [acts (fn [brd [x y z w]]
               (count
                 (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1]
                       :when (and (not (= 0 dx dy dz dw))
                                  (= \# (get brd [(+ x dx) (+ y dy) (+ z dz) (+ w dw)])))]
                   1)))
        cords (keys brd)
        xs (map first cords)
        [xl xh] [(apply min xs) (apply max xs)]
        ys (map second cords)
        [yl yh] [(apply min ys) (apply max ys)]
        zs (map #(nth % 2) cords)
        [zl zh] [(apply min zs) (apply max zs)]
        ws (map last cords)
        [wl wh] [(apply min ws) (apply max ws)]]
    (into {}
      (for [x (range (dec xl) (+ 2 xh))
            y (range (dec yl) (+ 2 yh))
            z (range (dec zl) (+ 2 zh))
            w (range (dec wl) (+ 2 wh))
            :let [pt [x y z w]
                  an (acts brd pt)]]
        (if (= \# (get brd pt \.))
          (if (<= 2 an 3) [pt \#])
          (if (= an 3) [pt \#]))))))

(defn two
  "Function to take a 3D board and make it 4D, and then run it through the
  4D evolution system 6 times to see what's left."
  [& [m]]
  (loop [brd (into {} (for [[k v] (or m puzzle)] [(conj k 0) v]))
         cnt 0]
    (if (< cnt 6)
      (recur (evolve2 brd) (inc cnt))
      (count brd))))

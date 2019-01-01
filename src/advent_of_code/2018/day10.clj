(ns advent-of-code.2018.day10
  "Tenth day's solutions for the Advent of Code 2018"
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def puzzle
  "These are the measures point positions and velocities for the input to the
  puzzle."
  (->> "resources/2018/input/day10.txt"
    (io/reader)
    (line-seq)))

(defn code-it
  "Function to turn a line - or the entire puzzle input - into a reasonably
  set of data structures for processing."
  [src]
  (let [re #"^position=<\s*(-{0,1}\d+),\s*(-{0,1}\d+)>\s+velocity=<\s*(-{0,1}\d+),\s*(-{0,1}\d+)>$"
        pint (fn [s] (Integer/parseInt (cs/trim s)))]
    (cond
      (string? src) (split-at 2 (map pint (drop 1 (re-matches re src))))
      (coll? src)   (map code-it src)
      :else         src)))

(defn limits
  "Function to return the limits on the provided set of ordered pairs: [x y]
  in a way that makes it easy to get the limits of x and y by this one call and
  a little destructuring."
  [s]
  (let [ps (map first s)]
    [(apply min (map first ps))
     (apply min (map second ps))
     (apply max (map first ps))
     (apply max (map second ps))]))

(defn area
  "Function to compute the rectangular area to enclose the points in the space."
  [s]
  (let [[xo yo xm ym] (limits s)]
    (* (- xm xo) (- ym yo))))

(defn adv
  "Function to take a map of positions with their particle velocities, and
  advance the entire system one second, and update the keys with their new
  positions based on the movement."
  [m]
  (doall (for [[[x y] [dx dy]] m] [[(+ x dx) (+ y dy)] [dx dy]])))

(defn img
  "Function to take a sequence of particle tracks, and grab the locations of
  each particle, and then plot it on an acsii graph. Nothing fancy, but the
  size will be key."
  [pts & [xl xh yl yh]]
  (let [ps (map first pts)
        xo (or xl (apply min (map first ps)))
        yo (or yl (apply min (map second ps)))
        xm (or xh (apply max (map first ps)))
        ym (or yh (apply max (map second ps)))
        w (inc (- xm xo))
        out (atom [])]
    (doseq [y (range yo (inc ym))]
      (let [row (atom (vec (repeat w ".")))]
        (doseq [xp (map first (filter #(= y (second %)) ps))]
          (swap! row assoc (- xp xo) "#"))
        (swap! out conj (apply str @row))))
    @out))

(defn two
  "Function to find the minimum area in the first 20,000 secs of
  evolution of the pattern, and then return the iteration on which
  occurred."
  [& [src]]
  (let [max-t 20000
        pts (code-it (or src puzzle))]
    (->> (iterate adv pts)
      (take max-t)
      (map area)
      (map vector (range))
      (sort-by second)
      (first)
      (first))))

(defn one
  "Function to use `two` to find the minimum area in the first 20,000 secs of
  evolution of the pattern, and then return the iteration on which
  occurred. This is then use to find that state, and then turn it into an
  image."
  [& [src]]
  (let [pts (code-it (or src puzzle))
        tgt (two pts)
        ans (first (drop tgt (iterate adv pts)))]
    (img ans)))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [max-t 6
        pts (code-it ["position=< 9,  1> velocity=< 0,  2>"
                      "position=< 7,  0> velocity=<-1,  0>"
                      "position=< 3, -2> velocity=<-1,  1>"
                      "position=< 6, 10> velocity=<-2, -1>"
                      "position=< 2, -4> velocity=< 2,  2>"
                      "position=<-6, 10> velocity=< 2, -2>"
                      "position=< 1,  8> velocity=< 1, -1>"
                      "position=< 1,  7> velocity=< 1,  0>"
                      "position=<-3, 11> velocity=< 1, -2>"
                      "position=< 7,  6> velocity=<-1, -1>"
                      "position=<-2,  3> velocity=< 1,  0>"
                      "position=<-4,  3> velocity=< 2,  0>"
                      "position=<10, -3> velocity=<-1,  1>"
                      "position=< 5, 11> velocity=< 1, -2>"
                      "position=< 4,  7> velocity=< 0, -1>"
                      "position=< 8, -2> velocity=< 0,  1>"
                      "position=<15,  0> velocity=<-2,  0>"
                      "position=< 1,  6> velocity=< 1,  0>"
                      "position=< 8,  9> velocity=< 0, -1>"
                      "position=< 3,  3> velocity=<-1,  1>"
                      "position=< 0,  5> velocity=< 0, -1>"
                      "position=<-2,  2> velocity=< 2,  0>"
                      "position=< 5, -2> velocity=< 1,  2>"
                      "position=< 1,  4> velocity=< 2,  1>"
                      "position=<-2,  7> velocity=< 2, -2>"
                      "position=< 3,  6> velocity=<-1, -1>"
                      "position=< 5,  0> velocity=< 1,  0>"
                      "position=<-6,  0> velocity=< 2,  0>"
                      "position=< 5,  9> velocity=< 1, -2>"
                      "position=<14,  7> velocity=<-2,  0>"
                      "position=<-3,  6> velocity=< 2, -1>"])
        tgt (->> (iterate adv pts)
              (take max-t)
              (map area)
              (map vector (range))
              (sort-by second)
              (first)
              (first))
        ans (first (drop tgt (iterate adv pts)))]
    (img ans)))

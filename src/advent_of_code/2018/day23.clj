(ns advent-of-code.2018.day23
  "Twenty-third day's solutions for the Advent of Code 2018"
  (:require [advent-of-code.path :refer [dist]]
            [advent-of-code.util :refer [rev parse-int]]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the location and radius data for all of the nanobots."
  (-> (slurp "resources/2018/input/day23.txt")
    (cs/trim)
    (cs/split #"\n")
    (code-it)))

(def sample
  "This is the sample nanobot data for testing."
  (-> ["pos=<0,0,0>, r=4"
       "pos=<1,0,0>, r=1"
       "pos=<4,0,0>, r=3"
       "pos=<0,2,0>, r=1"
       "pos=<0,5,0>, r=3"
       "pos=<0,0,3>, r=1"
       "pos=<1,1,1>, r=1"
       "pos=<1,1,2>, r=1"
       "pos=<1,3,1>, r=1"]
    (code-it)))

(defn code-it
  "Function to take the string data of the puzzle definition, and parse it into
  a usable data structure for processing."
  [s]
  (let [fix (fn [[x y z r]] {:pos [x y z] :radius r})]
    (cond
      (string? s) (->> (re-matches #"^pos=<(-{0,1}\d+),(-{0,1}\d+),(-{0,1}\d+)>, r=(\d+)$" s)
                    (drop 1)
                    (map parse-int)
                    (fix))
      (coll? s)   (map code-it s)
      :else       s)))

(defn exhaustive
  "Function to take a range in x, y, and z and scan every point in that area
  for the number of nanobots within range of that point. It then sorts them
  by the count of nanobots, and returns the top 10 in that region."
  [[xl xh] [yl yh] [zl zh]]
  (let [nbs puzzle
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))]
    (infof "exhaustive: %s %s %s" [xl xh] [yl yh] [zl zh])
    (->> (for [x (range xl (inc xh))
               y (range yl (inc yh))
               z (range zl (inc zh))
              :let [tp [x y z]
                    irc (count (filter #(rng? tp %) nbs))]
              :when (pos? irc)]
           {:pos tp :count irc :dist (dist [0 0 0] tp)})
      (sort-by :count >)
      (take 10))))

(defn contents
  "Function to return the count of nanobots that have some 'influence' within
  the region defined by the limits provided. This will be a component of the
  search system."
  [[xl xh] [yl yh] [zl zh]]
  (let [rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        nbc (loop [nbs puzzle
                   cnt 0]
              (if-let [nb (first nbs)]
                (let [hit? (or (rng? [xl yl zl] nb) (rng? [xh yl zl] nb)
                               (rng? [xl yh zl] nb) (rng? [xh yh zl] nb)
                               (rng? [xl yl zh] nb) (rng? [xh yl zh] nb)
                               (rng? [xl yh zh] nb) (rng? [xh yh zh] nb))]
                  (recur (rest nbs) (if hit? (inc cnt) cnt)))
                cnt))]
    {:count nbc :rx [xl xh] :ry [yl yh] :rz [zl zh]}))

(defn one
  "Function to calculate the number of nanobots within the range of the
  strongest field of one nanobot."
  []
  (let [nbs puzzle
        {rp :pos sr :radius} (first (sort-by :radius > nbs))]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))))

(defn two
  "Function to search the complete space and find the location - really, the
  distance from [0 0 0], that is within range of the highest number of
  nanobots. It's a basic binary search, in volume, and then when we get to
  32 points, we do a point-by-point search."
  []
  (let [nbs puzzle
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        box (fn [[xl xh] [yl yh] [zl zh]] (let [xm (int (Math/floor (/ (+ xh xl) 2)))
                                                ym (int (Math/floor (/ (+ yh yl) 2)))
                                                zm (int (Math/floor (/ (+ zh zl) 2)))]
                                            (->> [(contents [xl xm] [yl ym] [zl zm])
                                                  (contents [xm xh] [yl ym] [zl zm])
                                                  (contents [xl xm] [ym yh] [zl zm])
                                                  (contents [xm xh] [ym yh] [zl zm])
                                                  (contents [xl xm] [yl ym] [zm zh])
                                                  (contents [xm xh] [yl ym] [zm zh])
                                                  (contents [xl xm] [ym yh] [zm zh])
                                                  (contents [xm xh] [ym yh] [zm zh])]
                                              (sort-by :count >)
                                              (first))))]
    (-> (loop [rx [xl xh]
               ry [yl yh]
               rz [zl zh]]
          (let [{nrx :rx nry :ry nrz :rz cnt :count :as nb} (box rx ry rz)
                dx (apply - (reverse rx))
                dy (apply - (reverse ry))
                dz (apply - (reverse rz))
                dmin (min dx dy dz)]
            (infof "dx,dy,dz = %s, %s, %s ... count: %s" dx dy dz cnt)
            (if (< 32 dmin)
              (recur nrx nry nrz)
              nb)))
      (as-> s (exhaustive (:rx s) (:ry s) (:rz s))))))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [nbs sample
        {rp :pos sr :radius} (first (sort-by :radius > nbs))]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))))

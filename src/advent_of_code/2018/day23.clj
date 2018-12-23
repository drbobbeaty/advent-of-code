(ns advent-of-code.2018.day23
  "Twenty-third day's solutions for the Advent of Code 2018"
  (require [advent-of-code.2016.day25 :refer [->int]]
           [clojure.java.io :as io]
           [clojure.string :as cs]
           [clojure.tools.logging :refer [error errorf info infof warnf debugf]]))

(def puzzle
  "This is the location and radius data for all of the nanobots."
  (-> (slurp "resources/2018/input/day23.txt")
      (cs/trim)
      (cs/split #"\n")))

(def sample
  "This is the sample nanobot data for testing."
  ["pos=<0,0,0>, r=4"
   "pos=<1,0,0>, r=1"
   "pos=<4,0,0>, r=3"
   "pos=<0,2,0>, r=1"
   "pos=<0,5,0>, r=3"
   "pos=<0,0,3>, r=1"
   "pos=<1,1,1>, r=1"
   "pos=<1,1,2>, r=1"
   "pos=<1,3,1>, r=1"])

(def cluster
  "This is "
  ["pos=<10,12,12>, r=2"
   "pos=<12,14,12>, r=2"
   "pos=<16,12,12>, r=4"
   "pos=<14,14,14>, r=6"
   "pos=<50,50,50>, r=200"
   "pos=<10,10,10>, r=5"])

(defn code-it
  "Function to take the string data of the puzzle definition, and parse it into
  a usable data structure for processing."
  [s]
  (let [fix (fn [[x y z r]] {:pos [x y z] :radius r})]
    (cond
      (string? s) (->> (re-matches #"^pos=<(-{0,1}\d+),(-{0,1}\d+),(-{0,1}\d+)>, r=(\d+)$" s)
                    (drop 1)
                    (map ->int)
                    (fix))
      (coll? s)   (map code-it s)
      :else       s)))

(defn dist
  "Function to compute the Manhattan distance between the two points
  provided. This is just the simple distance in the coordinates."
  [p rp]
  (let [d (fn [[a b]] (Math/abs (- a b)))]
    (->> (map vector p rp)
      (map d)
      (apply +))))

(defn one
  "Function to calculate the number of nanobots within the range of the
  strongest field of one nanobot."
  []
  (let [nbs (code-it puzzle)
        {rp :pos sr :radius} (first (sort-by :radius > nbs))]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))))

(defn two
  ""
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        fo2-diff (/ (loop [d 1] (if (< d (- xh xl)) (recur (* 2 d)) d)) 2)
        ]
    (->> (for [z (range zl (inc zh))
               y (range yl (inc yh))
               x (range xl (inc xh))
               :let [tp [x y z]
                     irc (count (filter #(rng? tp %) nbs))]
               :when (pos? irc)]
           {:pos tp :count irc})
      (sort-by :count >)
      (first)
      (:pos)
      (dist [0 0 0])
      )
    ))

(defn yoyo
  "Function just to test out the example and make sure we have the tools
  working right."
  []
  (let [nbs (code-it sample)
        {rp :pos sr :radius} (first (sort-by :radius > nbs))
       ]
    (count (filter #(<= (dist rp (:pos %)) sr) nbs))
    ))

(defn bobo
  ""
  []
  (let [nbs (code-it cluster)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        [xl xh] (apply (juxt min max) (map #(nth (:pos %) 0) nbs))
        [yl yh] (apply (juxt min max) (map #(nth (:pos %) 1) nbs))
        [zl zh] (apply (juxt min max) (map #(nth (:pos %) 2) nbs))
        bg (fn [rx ry rz js] (let [dx (max 1 (/ js 2))
                                   calc (for [z (range (first rz) (second rz) dx)
                                              y (range (first ry) (second ry) dx)
                                              x (range (first rx) (second rx) dx)
                                             :let [tp [x y z]
                                                   irc (count (filter #(rng? tp %) nbs))]
                                             :when (pos? irc)]
                                          {:pos tp :count irc :dist (dist [0 0 0] tp)})
                                   hcnt (apply max (map :count calc))
                                   ]
                               (->> (filter #(= hcnt (:count %)) calc)
                                 (sort-by :dist)
                                 (first))
                               ))
                               ]
    (->> (loop [jmp (/ (loop [d 1] (if (< d (- xh xl)) (recur (* 2 d)) d)) 8)
                rx [xl (inc xh)]
                ry [yl (inc yh)]
                rz [zl (inc zh)]]
           (let [{[rpx rpy rpz] :pos cnt :count :as nb} (bg rx ry rz jmp)]
             (infof "jmp: %s ... nb: %s" jmp nb)
             (if (< 1 jmp)
               (recur (/ jmp 2) [(- rpx jmp) (+ rpx jmp)] [(- rpy jmp) (+ rpy jmp)] [(- rpz jmp) (+ rpz jmp)])
               nb))
           )
      ; (:pos)
      ; (dist [0 0 0])
      )
    ))

(defn refine
  ""
  []
  (let [nbs (code-it puzzle)
        rng? (fn [p {rp :pos sr :radius :as nb}] (<= (dist p rp) sr))
        [bgx bgy bgz] [13210274 27101304 37540631]
        [xl xh] [(- bgx 20) (inc (+ bgx 20))]
        [yl yh] [(- bgy 20) (inc (+ bgy 20))]
        [zl zh] [(- bgz 20) (inc (+ bgz 20))]
       ]
    (->> (for [x (range xl xh 2)
               y (range yl yh 2)
               z (range zl zh 2)
              :let [tp [x y z]
                    irc (count (filter #(rng? tp %) nbs))]
              :when (pos? irc)]
           {:pos tp :count irc :dist (dist [0 0 0] tp)})
      (sort-by :count >)
      )
    ))
